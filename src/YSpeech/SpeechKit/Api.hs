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
import Data.Aeson (encode, eitherDecode, FromJSON(..), withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
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
import System.FilePath (takeExtension)
import System.IO (hFlush, stderr)

import YSpeech.Types
import YSpeech.SpeechKit.Types

sttHost :: String
sttHost = "https://stt.api.cloud.yandex.net:443"

operationHost :: String
operationHost = "https://operation.api.cloud.yandex.net"

-- | Read a chunk file and submit it for async recognition.
-- Returns the operation ID.
recognizeFile :: Manager -> AppConfig -> AudioChunk -> IO Text
recognizeFile mgr cfg chunk = do
  audioBytes <- BS.readFile (chunkFilePath chunk)
  let ctype = detectContainerType (chunkFilePath chunk)
      body = encode $ mkRecognizeRequest audioBytes ctype (acModel cfg) (acLanguage cfg)
  req0 <- parseRequest $ sttHost ++ "/stt/v3/recognizeFileAsync"
  let req = addAuth cfg $ req0
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-folder-id", TE.encodeUtf8 (acFolderId cfg))
            ] ++ requestHeaders req0
        }
  resp <- httpLbs req mgr
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
-- The REST endpoint returns NDJSON: one JSON object per line,
-- each wrapped in {"result": <StreamingResponse>}.
getRecognition :: Manager -> AppConfig -> Text -> IO [StreamingResponse]
getRecognition mgr cfg opId' = do
  req0 <- parseRequest $ sttHost ++ "/stt/v3/getRecognition?operation_id="
                        ++ T.unpack opId'
  let req = addAuth cfg req0
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
      respBody = responseBody resp
  if status >= 200 && status < 300
    then parseNDJSON respBody
    else throwIO $ ApiRequestError status (decodeBody respBody)

-- | Parse NDJSON response where each line is {"result": <StreamingResponse>}.
parseNDJSON :: LBS.ByteString -> IO [StreamingResponse]
parseNDJSON body = do
  let ls = filter (not . LBS.null) $ LBS.split 0x0A body  -- split on newline
      parse1 line = case eitherDecode line of
        Right wrapper -> pure (ndjResult wrapper)
        Left e -> throwIO $ ResultParseError $
                    "Cannot parse NDJSON line: " <> T.pack e
                    <> " | line: " <> T.pack (take 200 (show line))
  mapM parse1 ls

-- | Wrapper for {"result": ...} NDJSON envelope.
data NDJSONWrapper = NDJSONWrapper { ndjResult :: StreamingResponse }

instance FromJSON NDJSONWrapper where
  parseJSON = withObject "NDJSONWrapper" $ \o ->
    NDJSONWrapper <$> o .: "result"

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

-- | Detect SpeechKit container type from file extension.
detectContainerType :: FilePath -> Text
detectContainerType fp =
  case map toLower (takeExtension fp) of
    ".ogg"  -> "OGG_OPUS"
    ".opus" -> "OGG_OPUS"
    ".wav"  -> "WAV"
    _       -> "MP3"
