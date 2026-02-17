{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative (execParser)
import System.IO (hFlush, stderr)
import System.IO.Temp (withSystemTempDirectory)

import YSpeech.Types
import YSpeech.Config
import YSpeech.Audio.Split
import YSpeech.SpeechKit.Api
import YSpeech.SpeechKit.Types ()
import YSpeech.Result.Merge
import YSpeech.Result.Format

main :: IO ()
main = do
  opts <- execParser optsInfo
  cfg  <- buildConfig opts
  mgr  <- newManager tlsManagerSettings

  withSystemTempDirectory "yspeech" $ \tmpDir -> do

    -- Phase 1: Split audio
    info' "Analysing input file..."
    chunks <- splitAudio (acInputFile cfg) (acChunkSeconds cfg) tmpDir
    info' $ "Split into " <> showT (length chunks) <> " chunk(s)"

    -- Phase 2: Submit recognition for each chunk (sends bytes directly)
    info' "Submitting recognition requests..."
    opIds <- mapConcurrently
               (\chunk -> do
                 oid <- recognizeFile mgr cfg chunk
                 info' $ "  Chunk " <> showT (chunkIndex chunk)
                       <> ": operation " <> oid
                 pure oid)
               chunks
    let chunkOps = zip chunks opIds

    -- Phase 3: Wait for all operations to complete
    info' "Waiting for recognition to complete..."
    _ <- mapConcurrently
           (\(_chunk, oid) -> do
             waitForCompletion mgr cfg oid
             info' $ "  Operation " <> oid <> " completed")
           chunkOps

    -- Phase 4: Fetch results
    info' "Fetching results..."
    results <- mapConcurrently
                 (\(_chunk, oid) -> getRecognition mgr cfg oid)
                 chunkOps
    let chunkResults = zip chunks results

    -- Phase 5: Merge + format
    let utterances = mergeResults chunkResults
        transcript = formatTranscript (acTimestamps cfg) utterances

    -- Phase 6: Output
    case acOutputFile cfg of
      Nothing   -> TIO.putStr transcript
      Just path -> do
        TIO.writeFile path transcript
        info' $ "Transcript written to " <> T.pack path

    info' "Done."

info' :: Text -> IO ()
info' msg = do
  TIO.hPutStrLn stderr msg
  hFlush stderr

showT :: Show a => a -> Text
showT = T.pack . show
