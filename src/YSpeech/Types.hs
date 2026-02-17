{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module YSpeech.Types
  ( AppConfig(..)
  , AuthMethod(..)
  , AudioChunk(..)
  , Utterance(..)
  , YSpeechError(..)
  ) where

import Control.Exception (Exception)
import Data.Text (Text)

data AuthMethod
  = AuthIAMToken Text
  | AuthApiKey Text
  deriving (Show)

data AppConfig = AppConfig
  { acInputFile    :: FilePath
  , acOutputFile   :: Maybe FilePath
  , acAuthMethod   :: AuthMethod
  , acFolderId     :: Text
  , acLanguage     :: Text      -- default "ru-RU"
  , acModel        :: Text      -- default "general"
  , acChunkSeconds :: Int       -- default 12600 (3.5 hours)
  , acTimestamps   :: Bool      -- include timestamps in output
  , acVerbose      :: Bool
  } deriving (Show)

data AudioChunk = AudioChunk
  { chunkIndex     :: Int
  , chunkFilePath  :: FilePath
  , chunkStartTime :: Double    -- seconds offset from original audio start
  , chunkDuration  :: Double    -- seconds
  } deriving (Show)

data Utterance = Utterance
  { uttSpeaker     :: Text      -- "Спикер 1", "Спикер 2"
  , uttText        :: Text
  , uttStartTimeMs :: Int       -- global timeline, milliseconds
  , uttEndTimeMs   :: Int
  } deriving (Show)

data YSpeechError
  = AudioSplitError Text
  | AudioProbeError Text
  | ApiRequestError Int Text        -- HTTP status + body
  | RecognitionError Text Text      -- operation ID + error message
  | ResultParseError Text
  | TimeoutError Text
  | ConfigError Text
  deriving (Show)

instance Exception YSpeechError
