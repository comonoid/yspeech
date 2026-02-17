{-# LANGUAGE OverloadedStrings #-}

module YSpeech.Audio.Split
  ( splitAudio
  , probeDuration
  ) where

import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.Process.Typed
    ( proc, readProcess, setStdin, closed )

import YSpeech.Types

-- | Probe the total duration of an audio file in seconds.
probeDuration :: FilePath -> IO Double
probeDuration path = do
  (exitCode, out, err) <- readProcess $
    setStdin closed $
    proc "ffprobe"
      [ "-v", "quiet"
      , "-show_entries", "format=duration"
      , "-of", "csv=p=0"
      , path
      ]
  case exitCode of
    ExitFailure c ->
      throwIO $ AudioProbeError $
        "ffprobe exited with code " <> showT c <> ": " <> bsToText err
    ExitSuccess ->
      case reads (filter (not . isSpace) (bsToString out)) of
        [(d, "")] -> pure d
        _         -> throwIO $ AudioProbeError $
                       "Cannot parse ffprobe output: " <> bsToText out

-- | Probe the number of audio channels.
probeChannels :: FilePath -> IO Int
probeChannels path = do
  (exitCode, out, err) <- readProcess $
    setStdin closed $
    proc "ffprobe"
      [ "-v", "quiet"
      , "-show_entries", "stream=channels"
      , "-select_streams", "a:0"
      , "-of", "csv=p=0"
      , path
      ]
  case exitCode of
    ExitFailure c ->
      throwIO $ AudioProbeError $
        "ffprobe channels exited with code " <> showT c <> ": " <> bsToText err
    ExitSuccess ->
      case reads (filter (not . isSpace) (bsToString out)) of
        [(n, "")] -> pure n
        _         -> pure 2  -- assume stereo if can't parse

-- | Split an MP3 file into chunks, converting to mono if needed.
-- If the input is already mono and fits in one chunk, returns it as-is.
-- Otherwise uses ffmpeg to split and/or downmix to mono.
splitAudio :: FilePath    -- ^ Input MP3 file
           -> Int         -- ^ Chunk duration in seconds
           -> FilePath    -- ^ Temporary directory for output chunks
           -> IO [AudioChunk]
splitAudio inputFile chunkSec tmpDir = do
  totalDur <- probeDuration inputFile
  channels <- probeChannels inputFile

  let needsMono = channels > 1
      needsSplit = totalDur > fromIntegral chunkSec + 60

  case (needsSplit, needsMono) of
    -- Already mono, fits in one chunk: use original file
    (False, False) ->
      pure [AudioChunk 0 inputFile 0 totalDur]

    -- Already mono, needs splitting: segment with stream copy
    (True, False) -> do
      let outPattern = tmpDir </> "chunk_%04d.mp3"
      runFfmpeg
        [ "-i", inputFile
        , "-f", "segment"
        , "-segment_time", show chunkSec
        , "-c", "copy"
        , "-reset_timestamps", "1"
        , outPattern
        ]
      collectChunks tmpDir chunkSec totalDur

    -- Stereo, fits in one chunk: just downmix to mono
    (False, True) -> do
      let outPath = tmpDir </> "mono.mp3"
      runFfmpeg
        [ "-i", inputFile
        , "-ac", "1"
        , "-c:a", "libmp3lame", "-q:a", "2"
        , outPath
        ]
      dur <- probeDuration outPath
      pure [AudioChunk 0 outPath 0 dur]

    -- Stereo and needs splitting: downmix + segment in one pass
    (True, True) -> do
      let outPattern = tmpDir </> "chunk_%04d.mp3"
      runFfmpeg
        [ "-i", inputFile
        , "-ac", "1"
        , "-c:a", "libmp3lame", "-q:a", "2"
        , "-f", "segment"
        , "-segment_time", show chunkSec
        , "-reset_timestamps", "1"
        , outPattern
        ]
      collectChunks tmpDir chunkSec totalDur

runFfmpeg :: [String] -> IO ()
runFfmpeg args = do
  (exitCode, _out, err) <- readProcess $
    setStdin closed $
    proc "ffmpeg" (["-v", "warning", "-y"] ++ args)
  case exitCode of
    ExitFailure c ->
      throwIO $ AudioSplitError $
        "ffmpeg exited with code " <> showT c <> ": " <> bsToText err
    ExitSuccess -> pure ()

collectChunks :: FilePath -> Int -> Double -> IO [AudioChunk]
collectChunks tmpDir chunkSec totalDur = do
  files <- sort . filter isMP3 <$> listDirectory tmpDir
  mapM (mkChunk tmpDir chunkSec totalDur) (zip [0..] files)

mkChunk :: FilePath -> Int -> Double -> (Int, FilePath) -> IO AudioChunk
mkChunk tmpDir chunkSec totalDur (idx, fileName) = do
  let path = tmpDir </> fileName
      startTime = fromIntegral idx * fromIntegral chunkSec
  dur <- probeDuration path
  let adjustedDur = min dur (totalDur - startTime)
  pure AudioChunk
    { chunkIndex     = idx
    , chunkFilePath  = path
    , chunkStartTime = startTime
    , chunkDuration  = adjustedDur
    }

isMP3 :: FilePath -> Bool
isMP3 f = takeExtension f == ".mp3"

showT :: Show a => a -> Text
showT = T.pack . show

bsToText :: LBS.ByteString -> Text
bsToText = T.pack . map (toEnum . fromEnum) . LBS.unpack

bsToString :: LBS.ByteString -> String
bsToString = map (toEnum . fromEnum) . LBS.unpack
