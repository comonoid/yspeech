{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module YSpeech.SpeechKit.Api
  ( recognizeFile
  , pollOperation
  , getRecognition
  , waitForCompletion
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import qualified Codec.Compression.GZip as GZip
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
    ( Manager, Request(..), RequestBody(..), Response
    , httpLbs, responseStatus, responseBody
    , parseRequest
    )
import Network.HTTP.Types.Status (statusCode)
import System.IO (hFlush, stderr)

import YSpeech.Types
import YSpeech.SpeechKit.Types

sttHost :: String
sttHost = "https://stt.api.cloud.yandex.net:443"

operationHost :: String
operationHost = "https://operation.api.cloud.yandex.net"

-- | Read a chunk file and submit it for async recognition.
-- Tries gzip-compressed request first; falls back to uncompressed on 415.
-- Returns the operation ID.
recognizeFile :: Manager -> AppConfig -> AudioChunk -> IO Text
recognizeFile mgr cfg chunk = do
  audioBytes <- BS.readFile (chunkFilePath chunk)
  let body = encode $ mkRecognizeRequest audioBytes (acModel cfg) (acLanguage cfg)
      gzBody = GZip.compress body
  req0 <- parseRequest $ sttHost ++ "/stt/v3/recognizeFileAsync"
  let baseHeaders =
        [ ("Content-Type", "application/json")
        , ("x-folder-id", TE.encodeUtf8 (acFolderId cfg))
        ]
      gzReq = addAuth cfg $ req0
        { method = "POST"
        , requestBody = RequestBodyLBS gzBody
        , requestHeaders = ("Content-Encoding", "gzip") : baseHeaders
                           ++ requestHeaders req0
        }
  resp <- httpLbs gzReq mgr
  let status = statusCode (responseStatus resp)
  if status == 415
    then do
      logMsg "  gzip not supported by server, retrying uncompressed..."
      let plainReq = addAuth cfg $ req0
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = baseHeaders ++ requestHeaders req0
            }
      resp' <- httpLbs plainReq mgr
      parseOperationResponse resp'
    else
      parseOperationResponse resp

parseOperationResponse :: Response LBS.ByteString -> IO Text
parseOperationResponse resp = do
  let status = statusCode (responseStatus resp)
      respBody = responseBody resp
  if status >= 200 && status < 300
    then case eitherDecode respBody of
      Right op -> pure (opId op)
      Left e   -> throwIO $ ResultParseError $
                    "Cannot parse operation response: " <> T.pack e
    else throwIO $ ApiRequestError status (decodeBody respBody)

-- | Check the status of an operation.
pollOperation :: Manager -> AppConfig -> Text -> IO Operation
pollOperation mgr cfg opId' = do
  req0 <- parseRequest $ operationHost ++ "/operations/" ++ T.unpack opId'
  let req = addAuth cfg req0
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
      respBody = responseBody resp
  if status >= 200 && status < 300
    then case eitherDecode respBody of
      Right op -> pure op
      Left e   -> throwIO $ ResultParseError $
                    "Cannot parse poll response: " <> T.pack e
    else throwIO $ ApiRequestError status (decodeBody respBody)

-- | Fetch recognition results for a completed operation.
getRecognition :: Manager -> AppConfig -> Text -> IO [StreamingResponse]
getRecognition mgr cfg opId' = do
  req0 <- parseRequest $ sttHost ++ "/stt/v3/getRecognition?operation_id="
                        ++ T.unpack opId'
  let req = addAuth cfg req0
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
      respBody = responseBody resp
  if status >= 200 && status < 300
    then case eitherDecode respBody of
      Right srs -> pure srs
      Left e    -> throwIO $ ResultParseError $
                     "Cannot parse recognition results: " <> T.pack e
    else throwIO $ ApiRequestError status (decodeBody respBody)

-- | Wait for an operation to complete, polling every 30 seconds.
waitForCompletion :: Manager -> AppConfig -> Text -> IO ()
waitForCompletion mgr cfg opId' = go (0 :: Int)
  where
    maxPolls   = 720     -- 720 * 30s = 6 hours
    pollDelay  = 30      -- seconds

    go n | n >= maxPolls =
           throwIO $ TimeoutError $
             "Operation " <> opId' <> " did not complete within timeout"
    go n = do
      op <- pollOperation mgr cfg opId'
      case opError op of
        Just e  -> throwIO $ RecognitionError opId' (oeMessage e)
        Nothing
          | opDone op -> pure ()
          | otherwise -> do
              threadDelay (pollDelay * 1_000_000)
              go (n + 1)

addAuth :: AppConfig -> Request -> Request
addAuth cfg req = req
  { requestHeaders = authHeader (acAuthMethod cfg) : requestHeaders req }
  where
    authHeader (AuthIAMToken t) = ("Authorization", "Bearer " <> TE.encodeUtf8 t)
    authHeader (AuthApiKey k)   = ("Authorization", "Api-Key " <> TE.encodeUtf8 k)

decodeBody :: LBS.ByteString -> Text
decodeBody = T.pack . take 1000 . show

logMsg :: Text -> IO ()
logMsg msg = TIO.hPutStrLn stderr msg >> hFlush stderr
