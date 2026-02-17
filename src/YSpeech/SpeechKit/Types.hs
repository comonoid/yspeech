{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module YSpeech.SpeechKit.Types
  ( -- * Request types
    RecognizeFileRequest(..)
  , RecognitionModelOptions(..)
  , AudioFormatOptions(..)
  , ContainerAudio(..)
  , SpeakerLabelingOptions(..)
  , LanguageRestrictionOptions(..)
    -- * Response types
  , Operation(..)
  , OperationError(..)
  , StreamingResponse(..)
  , FinalRefinement(..)
  , AlternativeUpdate(..)
  , Alternative(..)
  , RecWord(..)
    -- * Constructors
  , mkRecognizeRequest
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- Request types
----------------------------------------------------------------------

-- | Request body for POST /stt/v3/recognizeFileAsync.
-- Uses 'content' field to send audio bytes directly (base64-encoded in JSON).
data RecognizeFileRequest = RecognizeFileRequest
  { rfrContent          :: ByteString  -- raw audio bytes
  , rfrRecognitionModel :: RecognitionModelOptions
  , rfrSpeakerLabeling  :: SpeakerLabelingOptions
  } deriving (Show, Generic)

instance ToJSON RecognizeFileRequest where
  toJSON r = object
    [ "content"           .= TE.decodeUtf8 (B64.encode (rfrContent r))
    , "recognition_model" .= rfrRecognitionModel r
    , "speaker_labeling"  .= rfrSpeakerLabeling r
    ]

data RecognitionModelOptions = RecognitionModelOptions
  { rmoModel               :: Text
  , rmoAudioFormat         :: AudioFormatOptions
  , rmoAudioProcessingType :: Text
  , rmoLanguageRestriction :: LanguageRestrictionOptions
  } deriving (Show, Generic)

instance ToJSON RecognitionModelOptions where
  toJSON r = object
    [ "model"                 .= rmoModel r
    , "audio_format"          .= rmoAudioFormat r
    , "audio_processing_type" .= rmoAudioProcessingType r
    , "language_restriction"  .= rmoLanguageRestriction r
    ]

data AudioFormatOptions = AudioFormatOptions
  { afoContainerAudio :: ContainerAudio
  } deriving (Show, Generic)

instance ToJSON AudioFormatOptions where
  toJSON a = object ["container_audio" .= afoContainerAudio a]

data ContainerAudio = ContainerAudio
  { caType :: Text  -- "MP3", "WAV", "OGG_OPUS"
  } deriving (Show, Generic)

instance ToJSON ContainerAudio where
  toJSON c = object ["container_audio_type" .= caType c]

data SpeakerLabelingOptions = SpeakerLabelingOptions
  { sloSpeakerLabeling :: Text  -- "SPEAKER_LABELING_ENABLED"
  } deriving (Show, Generic)

instance ToJSON SpeakerLabelingOptions where
  toJSON s = object ["speaker_labeling" .= sloSpeakerLabeling s]

data LanguageRestrictionOptions = LanguageRestrictionOptions
  { lroRestrictionType :: Text    -- "WHITELIST"
  , lroLanguageCodes   :: [Text]  -- ["ru-RU"]
  } deriving (Show, Generic)

instance ToJSON LanguageRestrictionOptions where
  toJSON l = object
    [ "restriction_type" .= lroRestrictionType l
    , "language_code"    .= lroLanguageCodes l
    ]

-- | Build a recognition request with audio content embedded.
-- containerType: "MP3", "OGG_OPUS", or "WAV"
mkRecognizeRequest :: ByteString -> Text -> Text -> Text -> RecognizeFileRequest
mkRecognizeRequest audioBytes containerType model language = RecognizeFileRequest
  { rfrContent = audioBytes
  , rfrRecognitionModel = RecognitionModelOptions
      { rmoModel = model
      , rmoAudioFormat = AudioFormatOptions
          { afoContainerAudio = ContainerAudio containerType }
      , rmoAudioProcessingType = "FULL_DATA"
      , rmoLanguageRestriction = LanguageRestrictionOptions
          { lroRestrictionType = "WHITELIST"
          , lroLanguageCodes = [language]
          }
      }
  , rfrSpeakerLabeling = SpeakerLabelingOptions "SPEAKER_LABELING_ENABLED"
  }

----------------------------------------------------------------------
-- Response types
----------------------------------------------------------------------

data Operation = Operation
  { opId    :: Text
  , opDone  :: Bool
  , opError :: Maybe OperationError
  } deriving (Show, Generic)

instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \o -> Operation
    <$> o .:  "id"
    <*> o .:? "done" .!= False
    <*> o .:? "error"

data OperationError = OperationError
  { oeCode    :: Int
  , oeMessage :: Text
  } deriving (Show, Generic)

instance FromJSON OperationError where
  parseJSON = withObject "OperationError" $ \o -> OperationError
    <$> o .: "code"
    <*> o .:? "message" .!= ""

data StreamingResponse = StreamingResponse
  { srChannelTag      :: Maybe Text
  , srFinal           :: Maybe AlternativeUpdate
  , srFinalRefinement :: Maybe FinalRefinement
  } deriving (Show, Generic)

instance FromJSON StreamingResponse where
  parseJSON = withObject "StreamingResponse" $ \o -> StreamingResponse
    <$> o .:? "channelTag"
    <*> o .:? "final"
    <*> o .:? "finalRefinement"

data FinalRefinement = FinalRefinement
  { frFinalIndex     :: Maybe Text
  , frNormalizedText :: Maybe AlternativeUpdate
  } deriving (Show, Generic)

instance FromJSON FinalRefinement where
  parseJSON = withObject "FinalRefinement" $ \o -> FinalRefinement
    <$> o .:? "finalIndex"
    <*> o .:? "normalizedText"

data AlternativeUpdate = AlternativeUpdate
  { auAlternatives :: [Alternative]
  } deriving (Show, Generic)

instance FromJSON AlternativeUpdate where
  parseJSON = withObject "AlternativeUpdate" $ \o -> AlternativeUpdate
    <$> o .:? "alternatives" .!= []

data Alternative = Alternative
  { altText        :: Text
  , altWords       :: [RecWord]
  , altStartTimeMs :: Maybe Text  -- proto int64 as string
  , altEndTimeMs   :: Maybe Text
  , altConfidence  :: Maybe Double
  } deriving (Show, Generic)

instance FromJSON Alternative where
  parseJSON = withObject "Alternative" $ \o -> Alternative
    <$> o .:? "text" .!= ""
    <*> o .:? "words" .!= []
    <*> o .:? "startTimeMs"
    <*> o .:? "endTimeMs"
    <*> o .:? "confidence"

data RecWord = RecWord
  { rwText        :: Text
  , rwStartTimeMs :: Maybe Text
  , rwEndTimeMs   :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON RecWord where
  parseJSON = withObject "RecWord" $ \o -> RecWord
    <$> o .:? "text" .!= ""
    <*> o .:? "startTimeMs"
    <*> o .:? "endTimeMs"
