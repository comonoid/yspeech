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
import System.Directory (listDirectory, getFileSize)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.Process.Typed
    ( proc, readProcess, setStdin, nullStream )

import YSpeech.Types

-- | Probe the total duration of an audio file in seconds.
probeDuration :: FilePath -> IO Double
probeDuration path = do
  (exitCode, out, err) <- readProcess $
    setStdin nullStream $
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
    setStdin nullStream $
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

-- | Maximum byte size of a single chunk *file*. The SpeechKit
-- recognizeFileAsync endpoint rejects request bodies larger than 100 MiB.
-- We send the audio base64-encoded inside JSON, which inflates it by ~4/3,
-- so the file cap must be ~75 MiB to keep the body under 100 MiB. We use
-- 70 MiB to also absorb JSON overhead and VBR size variance between segments.
maxChunkBytes :: Integer
maxChunkBytes = 70 * 1024 * 1024

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
  fileSize <- getFileSize inputFile

  let needsMono = channels > 1
      -- Bytes per second of the input. SpeechKit (gRPC) rejects any single
      -- message above ~100 MiB, so duration is not the only limit: a short
      -- but high-bitrate file can still be too large. We also assume a
      -- re-encode never exceeds ~256 kbit/s mono, so the cap stays safe even
      -- when the input itself is low-bitrate (re-encoding could inflate it).
      inBytesPerSec  = fromIntegral fileSize / totalDur :: Double
      encBytesPerSec = 256 * 1000 / 8 :: Double
      bytesPerSec    = max inBytesPerSec encBytesPerSec
      -- Longest chunk that keeps a single API message under the size cap.
      sizeCapSec     = floor (fromIntegral maxChunkBytes / bytesPerSec) :: Int
      -- Effective chunk length: smaller of the user's time limit and the
      -- size-derived limit.
      effChunkSec    = max 1 (min chunkSec sizeCapSec)
      needsSplit     = totalDur > fromIntegral effChunkSec + 60

  case (needsSplit, needsMono) of
    -- Already mono, fits in one chunk (by time and size): use original file
    (False, False) ->
      pure [AudioChunk 0 inputFile 0 totalDur]

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

    -- Needs splitting (by time and/or size): re-encode to mono MP3 and
    -- segment in one pass. We always re-encode rather than stream-copy so
    -- the operation is correct for any input codec/container and so chunk
    -- sizes line up with the size cap computed above.
    (True, _) -> do
      let outPattern = tmpDir </> "chunk_%04d.mp3"
      runFfmpeg
        [ "-i", inputFile
        , "-ac", "1"
        , "-c:a", "libmp3lame", "-q:a", "2"
        , "-f", "segment"
        , "-segment_time", show effChunkSec
        , "-reset_timestamps", "1"
        , outPattern
        ]
      collectChunks tmpDir effChunkSec totalDur

runFfmpeg :: [String] -> IO ()
runFfmpeg args = do
  -- -nostdin is essential: we run ffmpeg with stdin redirected to /dev/null
  -- (setStdin nullStream). If stdin were instead *closed*, ffmpeg's input
  -- file would be assigned fd 0 and ffmpeg's interactive-key reader would
  -- consume bytes from it, corrupting the demuxer and producing an endless
  -- stream of bogus decode errors. -nostdin disables that reader outright.
  let prefix = [ "-v", "warning", "-y", "-nostdin" ]
  (exitCode, _out, err) <- readProcess $
    setStdin nullStream $
    proc "ffmpeg" (prefix ++ args)
  case exitCode of
    ExitFailure c ->
      throwIO $ AudioSplitError $
        "ffmpeg exited with code " <> showT c <> ": " <> lastLines 20 (bsToText err)
    ExitSuccess -> pure ()

-- | Keep only the last @n@ lines of a (possibly enormous) log, so a flood of
-- per-frame decoder warnings does not produce a multi-thousand-line exception.
lastLines :: Int -> Text -> Text
lastLines n txt =
  let ls = T.lines txt
      dropped = length ls - n
  in if dropped > 0
       then "(... " <> showT dropped <> " more line(s) ...)\n"
              <> T.unlines (drop dropped ls)
       else txt

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
