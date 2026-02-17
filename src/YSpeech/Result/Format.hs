{-# LANGUAGE OverloadedStrings #-}

module YSpeech.Result.Format
  ( formatTranscript
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import YSpeech.Types (Utterance(..))

-- | Format a list of utterances into a human-readable transcript.
-- Consecutive utterances from the same speaker are merged.
-- If withTimestamps is True, each line is prefixed with [HH:MM:SS].
formatTranscript :: Bool -> [Utterance] -> Text
formatTranscript withTs utts =
  T.intercalate "\n" $ map fmt (groupBySpeaker utts)
  where
    fmt = if withTs then formatGroupTs else formatGroup

-- | Group consecutive utterances by the same speaker.
groupBySpeaker :: [Utterance] -> [(Text, Int, Text)]
  -- (speaker, startTimeMs, combined text)
groupBySpeaker [] = []
groupBySpeaker (u:us) = go (uttSpeaker u) (uttStartTimeMs u) [uttText u] us
  where
    go spk startMs txts [] = [(spk, startMs, T.intercalate " " (reverse txts))]
    go spk startMs txts (x:xs)
      | uttSpeaker x == spk =
          go spk startMs (uttText x : txts) xs
      | otherwise =
          (spk, startMs, T.intercalate " " (reverse txts))
          : go (uttSpeaker x) (uttStartTimeMs x) [uttText x] xs

formatGroup :: (Text, Int, Text) -> Text
formatGroup (speaker, _startMs, txt) =
  speaker <> ": " <> txt

formatGroupTs :: (Text, Int, Text) -> Text
formatGroupTs (speaker, startMs, txt) =
  "[" <> formatTimestamp startMs <> "] " <> speaker <> ": " <> txt

formatTimestamp :: Int -> Text
formatTimestamp ms =
  let totalSec = ms `div` 1000
      h = totalSec `div` 3600
      m = (totalSec `mod` 3600) `div` 60
      s = totalSec `mod` 60
  in T.pack $ pad2 h ++ ":" ++ pad2 m ++ ":" ++ pad2 s

pad2 :: Int -> String
pad2 n
  | n < 10    = '0' : show n
  | otherwise = show n
