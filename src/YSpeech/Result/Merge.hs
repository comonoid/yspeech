{-# LANGUAGE OverloadedStrings #-}

module YSpeech.Result.Merge
  ( mergeResults
  ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import YSpeech.Types (AudioChunk(..), Utterance(..))
import YSpeech.SpeechKit.Types

-- | Merge recognition results from multiple chunks into a flat list
-- of utterances, sorted by global timestamp.
-- Adjusts timestamps by adding each chunk's start time offset.
mergeResults :: [(AudioChunk, [StreamingResponse])] -> [Utterance]
mergeResults = sortOn uttStartTimeMs . concatMap (uncurry chunkToUtterances)

chunkToUtterances :: AudioChunk -> [StreamingResponse] -> [Utterance]
chunkToUtterances chunk responses =
  let offsetMs = round (chunkStartTime chunk * 1000) :: Int
      -- Prefer final_refinement (normalized text) over raw final
      extract sr = case extractBestText sr of
        Nothing -> Nothing
        Just (speaker, txt, startMs, endMs) ->
          Just Utterance
            { uttSpeaker     = speakerLabel speaker
            , uttText        = txt
            , uttStartTimeMs = startMs + offsetMs
            , uttEndTimeMs   = endMs + offsetMs
            }
  in mapMaybe extract responses

extractBestText :: StreamingResponse -> Maybe (Text, Text, Int, Int)
extractBestText sr =
  let speaker = fromMaybe "" (srChannelTag sr)
      -- Use only finalRefinement (normalized text).
      -- Ignore raw 'final' to avoid duplicates.
      mAltUpdate = srFinalRefinement sr >>= frNormalizedText
  in case mAltUpdate of
    Nothing -> Nothing
    Just au -> case auAlternatives au of
      []    -> Nothing
      (a:_) ->
        let txt = altText a
            startMs = parseMs (altStartTimeMs a)
                        (listToMaybe (altWords a) >>= rwStartTimeMs)
            endMs   = parseMs (altEndTimeMs a)
                        (listToMaybe (reverse (altWords a)) >>= rwEndTimeMs)
        in if T.null (T.strip txt)
           then Nothing
           else Just (speaker, txt, startMs, endMs)

-- | Parse a millisecond value from proto int64 string representation.
-- Falls back to a secondary value, then to 0.
parseMs :: Maybe Text -> Maybe Text -> Int
parseMs primary secondary =
  case primary >>= (readMaybe . T.unpack) of
    Just n  -> n
    Nothing -> case secondary >>= (readMaybe . T.unpack) of
      Just n  -> n
      Nothing -> 0

-- | Map channel_tag to human-readable speaker label.
-- "0" -> "Спикер 1", "1" -> "Спикер 2", etc.
speakerLabel :: Text -> Text
speakerLabel tag =
  case readMaybe (T.unpack tag) :: Maybe Int of
    Just n  -> "Спикер " <> T.pack (show (n + 1))
    Nothing -> "Спикер ?"
