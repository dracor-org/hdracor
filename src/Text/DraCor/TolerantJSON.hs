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
import Control.Monad
import Control.Applicative
import Data.Either
import Data.Text.Read

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
  parseJSON = parseMetrics

parseMetrics :: Value -> Parser Metrics
parseMetrics (Object s) = Metrics
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
  -- workaround issue # of dracor-api
  <*> tolerateStringNum decimal s "networkSize" "networkSize" --  s .:? "networkSize"
  <*> s .:? "allInIndex"
  <*> s .:? "allInSegment"
  <*> s .:? "nodes" .!= []
parseMetrics _ = mzero

instance ToJSON Metrics


-- * JSON for 'Metadata'

instance FromJSON Metadata where
  parseJSON = parseMetadata

instance ToJSON Metadata

-- | Parser for 'Metadata'.
parseMetadata :: Value -> Parser Metadata
parseMetadata (Object s) = Metadata
  <$> s .: "id"
  <*> s .: "name"
  <*> s .:? "corpus"
  <*> s .:? "genre"
  <*> s .:? "title"
  <*> s .:? "subtitle"
  <*> s .:? "authors" .!= []
  -- Workaround for issue #88 of dracor-api:
  <*> (fmap (sourceName) $
       ((explicitParseFieldMaybe parseSource s "source") .!= (Source Nothing Nothing)))
  -- Workaround for issue #88 of dracor-api:
  <*> asum [ fmap Just $ s .: "sourceUrl" -- not .:?, because we want it failable
           , (fmap (sourceUrl) $
              ((explicitParseFieldMaybe parseSource s "source") .!= (Source Nothing Nothing)))
           ]
  <*> s .:? "originalSource"
  -- Workaround for issue #83 of dracor-api:
  <*> tolerateStringNum (signed decimal) s "yearPremiered" "premiereYear"
  <*> tolerateStringNum (signed decimal) s "yearPrinted" "printYear"
  <*> tolerateStringNum (signed decimal) s "yearNormalized" "normalizedYear"
  <*> tolerateStringNum (signed decimal) s "yearWritten" "writtenYear"
  <*> s .:? "wikidataId"
  <*> s .:? "networkdataCsvUrl"
parseMetadata _ = mzero
  
-- | Parse a numerical value or a string representing a numerical
-- value (in an other field).
tolerateStringNum :: (FromJSON a) =>
                     Reader a   -- ^ a reader from Data.Text.Read
                  -> Object     -- ^ the json object
                  -> T.Text     -- ^ the name of the numerical field
                  -> T.Text     -- ^ name of (alternative) text field
                  -> Parser (Maybe a)
tolerateStringNum reader v numField strField = asum
  [ fmap Just $ v .: numField -- not .:?, because we want it to fail if not present
  , fmap (join . (fmap ((either (pure Nothing) (Just . fst)) . reader))) $
    (v .:? strField :: Parser (Maybe T.Text))
  ]


-- * JSON for 'Play'

--  $(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Play)


instance ToJSON Play

instance FromJSON Play where
  parseJSON = parsePlay
  
parsePlay :: Value -> Parser Play
parsePlay v@(Object o) = Play
  <$> parseMetadata v
  <*> parseMetrics v
  <*> o .:? "segments" .!= []
  <*> o .:? "cast" .!= []
parsePlay _ = mzero


-- * JSON for 'Corpus'

instance ToJSON Corpus

instance FromJSON Corpus where
  parseJSON = parseCorpus

parseCorpus :: Value -> Parser Corpus
parseCorpus (Object o) = Corpus
  <$> o .:? "name"
  <*> o .:? "title"
  <*> o .:? "repository"
  <*> o .:? "uri"
  <*> o .:? "dramas" .!= []
parseCorpus _ = mzero
