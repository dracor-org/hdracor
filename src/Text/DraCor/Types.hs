{-# LANGUAGE TemplateHaskell, DeriveGeneric, DuplicateRecordFields #-}
module Text.DraCor.Types
  where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson.TH

import Text.DraCor.Utils




-- * Records for storing json data from the DraCor API



type URL = Text

type Year = Text
type YearInt = Int


data API = API
  { apiName :: Text
  , apiStatus :: Text
  , apiExistdb :: Text
  , apiVersion :: Text
  } deriving (Generic, Show, Eq)


$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 3} ''API)


data Node = Node
  { nodeWeightedDegree :: Maybe Int
  , nodeDegree :: Maybe Int
  , nodeCloseness :: Maybe Float
  , nodeEigenvector :: Maybe Float
  , nodeId :: Maybe Text
  , nodeBetweenness :: Maybe Float
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 4} ''Node)


data Scene = Scene
  { scnNumber :: Maybe Int
  , scnTitle :: Maybe Text
  , scnSpeakers :: Maybe [Text]
  , scnType :: Maybe Text
  } deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 3} ''Scene)
-- instance FromJSON Scene


data Source = Source
  { srcName :: Maybe Text
  , srcUrl :: Maybe URL
  } deriving (Generic, Show, Eq)

-- instance FromJSON Source
$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 3} ''Source)

data CastItem = CastItem
  { cstiSex :: Maybe Text
  , cstiName :: Maybe Text
  , cstiIsGroup :: Maybe Bool
  , cstiId :: Maybe Text
  } deriving (Generic, Show, Eq)

-- instance FromJSON CastItem
$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 4} ''CastItem)


data Author = Author
  { authrName :: Maybe Text
  , authrKey :: Maybe Text
  , warning :: Maybe Text -- deprecation warning
  } deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 5} ''Author)
-- instance FromJSON Author


data Play = Play
  { -- Metrics
    plySize :: Maybe Int
  , plyAverageClustering :: Maybe Float
  , plyNumOfPersonGroups :: Maybe Int
  , plyDensity :: Maybe Float
  , plyAveragePathLength :: Maybe Float
  , plyMaxDegreeIds :: Maybe Text  -- Maybe [Text]
  , plyAverageDegree :: Maybe Float
  , plyDiameter :: Maybe Int
  , plyMaxDegree :: Maybe Int
  , plyNumOfSpeakers :: Maybe Int
  , plyNumConnectedComponents :: Maybe Int
  , plyNumOfSpeakersFemale :: Maybe Int
  , plyNumOfSpeakersMale :: Maybe Int
  , plyNumOfSpeakersUnknown :: Maybe Int
  , plyNumOfSegments :: Maybe Int
  , plyWikipediaLinkCount :: Maybe Int
  , plyNumOfActs :: Maybe Int
  , plyNetworkSize :: Maybe Int
  , plyAllInIndex :: Maybe Float
  , plyAllInSegment :: Maybe Int
  -- meta data
  , plyId :: Maybe Text
  , plyName :: Maybe Text
  , plyPlayName :: Maybe Text
  , plyGenre :: Maybe Text
  , plyAuthors :: Maybe [Author]
  , plyAuthor :: Maybe Author
  , plyCorpus :: Maybe Text
  , plyOriginalSource :: Maybe Text
  , plyYearPremiered :: Maybe YearInt
  , plyYearPrinted :: Maybe YearInt
  , plyYearNormalized :: Maybe YearInt
  , plyYearWritten :: Maybe YearInt
  , plyWikidataId :: Maybe Text
  , plySubtitle :: Maybe Text
  , plyTitle :: Maybe Text
  , plySource :: Maybe Source
  , plySourceUrl :: Maybe URL
  , plyPrintYear :: Maybe Year
  , plyPremiereYear :: Maybe Year
  , plyWrittenYear :: Maybe Year
  , plyNetworkdataCsvUrl :: Maybe URL
  -- -- deprecated meta data, see issue #83 of dracor-api
  -- , plyPrintYear :: Maybe Text
  -- , plyPremiereYear :: Maybe Text
  -- , plyWrittenYear :: Maybe Text
  -- Scenes
  , plySegments :: Maybe [Scene]
  -- characters
  , plyCast :: Maybe [CastItem]
  -- Nodes
  , plyNodes :: Maybe [Node]
  } deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 3} ''Play)

-- needed for api fun /corpora/{name}
data PlayFromCorpusList = PlayFromCorpusList
  { plyflId :: Maybe Text
  , plyflName :: Maybe Text
  , plyflTitle :: Maybe Text
  , plyflSubtitle :: Maybe Text
  , plyflAuthors :: Maybe [Author]
  , plyflAuthor :: Maybe Author
  , plyflSource :: Maybe Text -- not Source
  , plyflSourceUrl :: Maybe URL
  , plyflYearNormalized :: Maybe Year -- Other type than in Play!!!
  , plyflPrintYear :: Maybe Year
  , plyflPremiereYear :: Maybe Year
  , plyflWrittenYear :: Maybe Year
  , plyflNetworkSize :: Maybe Text -- Int
  , plyflNetworkdataCsvUrl :: Maybe URL
  , plyflWikidataId :: Maybe Text
  } deriving (Generic, Show, Eq)

-- instance FromJSON Play
$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 5} ''PlayFromCorpusList)


data Corpus = Corpus
  { crpsName :: Maybe Text
  , crpsTitle :: Maybe Text
  , crpsRepository :: Maybe Text
  , crpsUri :: Maybe Text
  , crpsDramas :: Maybe [PlayFromCorpusList]  -- FIXME: should be Maybe [Play]
  } deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = modifyField 4} ''Corpus)
-- instance FromJSON Corpus


