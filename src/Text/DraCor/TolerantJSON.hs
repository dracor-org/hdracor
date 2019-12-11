{-# LANGUAGE OverloadedStrings #-}
module Text.DraCor.TolerantJSON
  where

-- | This module defines a fault tolerant set of parsers and
-- serializers for JSON. It has some work arounds for issues of the
-- dracor API.

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Aeson.TH as ATH
import Data.Foldable

import Text.DraCor.Types
import Text.DraCor.CommonJSON


-- * JSON for 'Metrics'

instance FromJSON Metrics where
  parseJSON = withObject "metrics" $ \s -> Metrics
    <$> s .:? "size"
    -- Workaround for issue #85 of dracor-api:
    <*> (fmap Just $ s .:? "averageClustering" .!= 0.0)
    <*> s .:? "numOfPersonGroups"
    <*> s .:? "density"
    <*> s .:? "averagePathLength"
    -- Workaround for issue #84 of dracor-api:
    <*> (asum
         [ s .:? "maxDegreeIds" -- alread a list of strings
         , fmap (fmap (T.splitOn "|")) $ s .:? "maxDegreeIds"
         ])
    <*> s .:? "averageDegree"
    <*> s .:? "diameter"
    <*> s .:? "maxDegree"
    <*> s .:? "numOfSpeakers"
    <*> s .:? "numConnectedComponents"
    <*> s .:? "numOfSpeakersFemale"
    <*> s .:? "numOfSpeakersMale"
    <*> s .:? "numOfSpeakersUnknown"
    <*> s .:? "numOfSegments"
    <*> s .:? "numOfActs"
    <*> s .:? "networkSize"
    <*> s .:? "allInIndex"
    <*> s .:? "allInSegment"

instance ToJSON Metrics

