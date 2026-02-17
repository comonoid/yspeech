{-# LANGUAGE OverloadedStrings #-}

module YSpeech.Config
  ( Options(..)
  , optionsParser
  , optsInfo
  , buildConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import System.Environment (lookupEnv)
import YSpeech.Types

data Options = Options
  { optInputFile  :: FilePath
  , optOutputFile :: Maybe FilePath
  , optFolderId   :: Maybe Text
  , optIAMToken   :: Maybe Text
  , optApiKey     :: Maybe Text
  , optLanguage   :: Text
  , optModel      :: Text
  , optChunkHours :: Double
  , optTimestamps :: Bool
  , optVerbose    :: Bool
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "input" <> short 'i'
     <> metavar "FILE"
     <> help "Input MP3 file" )
  <*> optional (strOption
      ( long "output" <> short 'o'
     <> metavar "FILE"
     <> help "Output file (stdout if omitted)" ))
  <*> optional (strOption
      ( long "folder-id"
     <> metavar "ID"
     <> help "Yandex Cloud folder ID (or YANDEX_FOLDER_ID env)" ))
  <*> optional (strOption
      ( long "iam-token"
     <> metavar "TOKEN"
     <> help "IAM token (or YANDEX_IAM_TOKEN env)" ))
  <*> optional (strOption
      ( long "api-key"
     <> metavar "KEY"
     <> help "API key (or YANDEX_API_KEY env)" ))
  <*> strOption
      ( long "language"
     <> metavar "LANG"
     <> value "ru-RU"
     <> showDefault
     <> help "Recognition language" )
  <*> strOption
      ( long "model"
     <> metavar "MODEL"
     <> value "general"
     <> showDefault
     <> help "Recognition model" )
  <*> option auto
      ( long "chunk-hours"
     <> metavar "HOURS"
     <> value 3.5
     <> showDefault
     <> help "Max chunk duration in hours" )
  <*> switch
      ( long "timestamps"
     <> help "Include timestamps in output" )
  <*> switch
      ( long "verbose" <> short 'v'
     <> help "Verbose output" )

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Recognize long audio files via Yandex SpeechKit"
 <> header "yspeech - Yandex SpeechKit async recognition CLI" )

buildConfig :: Options -> IO AppConfig
buildConfig opts = do
  folderId <- resolve "folder-id" (optFolderId opts) "YANDEX_FOLDER_ID"
  auth     <- resolveAuth (optIAMToken opts) (optApiKey opts)
  pure AppConfig
    { acInputFile    = optInputFile opts
    , acOutputFile   = optOutputFile opts
    , acAuthMethod   = auth
    , acFolderId     = folderId
    , acLanguage     = optLanguage opts
    , acModel        = optModel opts
    , acChunkSeconds = round (optChunkHours opts * 3600)
    , acTimestamps   = optTimestamps opts
    , acVerbose      = optVerbose opts
    }

resolve :: String -> Maybe Text -> String -> IO Text
resolve name mVal envVar = case mVal of
  Just v  -> pure v
  Nothing -> do
    env <- lookupEnv envVar
    case env of
      Just v  -> pure (T.pack v)
      Nothing -> fail $ "Missing " ++ name
                     ++ ". Set --" ++ name ++ " or " ++ envVar ++ " env var."

resolveAuth :: Maybe Text -> Maybe Text -> IO AuthMethod
resolveAuth (Just iam) _         = pure (AuthIAMToken iam)
resolveAuth _          (Just ak) = pure (AuthApiKey ak)
resolveAuth Nothing    Nothing   = do
  iam <- lookupEnv "YANDEX_IAM_TOKEN"
  case iam of
    Just t  -> pure (AuthIAMToken (T.pack t))
    Nothing -> do
      ak <- lookupEnv "YANDEX_API_KEY"
      case ak of
        Just k  -> pure (AuthApiKey (T.pack k))
        Nothing -> fail "Missing auth. Set --iam-token, --api-key, or YANDEX_IAM_TOKEN / YANDEX_API_KEY env var."
