{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
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
import Data.Aeson.Types

import Text.DraCor.Types
import Text.DraCor.CommonJSON
import Text.DraCor.Utils


-- * JSON for 'Source'

instance FromJSON Source where
  parseJSON = parseSource

parseSource :: Value -> Parser Source
parseSource (Object s) = Source
  <$> s .:? "name"
  <*> s .:? "url"
parseSource (String v) = SimpleSource
  <$> pure v
parseSource invalid =
  prependFailure "parsing \'source\' failed, " (typeMismatch "Object" invalid)

instance ToJSON Source


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


-- * JSON for 'Metadata'

instance FromJSON Metadata where
  parseJSON = parseMetadata

parseMetadata :: Value -> Parser Metadata
parseMetadata = withObject "metadata" $ \s -> Metadata
  <$> s .: "id"
  <*> s .: "name"
  <*> s .:? "corpus"
  <*> s .:? "genre"
  <*> s .:? "title"
  <*> s .:? "subtitle"
  <*> s .:? "authors" .!= []
  <*> s .:? "source"  -- FIXME
  <*> s .:? "sourceUrl"
  <*> s .:? "originalSource" -- deprecated?
  <*> s .:? "yearPremiered"
  <*> s .:? "yearPrinted"
  <*> s .:? "yearNormalized"
  <*> s .:? "yearWritten"
  <*> s .:? "wikidataId"
  <*> s .:? "networkdataCsvUrl"
  


-- * JSON for 'Play'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Play)


-- * JSON for 'PlayFromCorpusList'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 5} ''PlayFromCorpusList)


-- * JSON for 'Corpus'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 4} ''Corpus)
