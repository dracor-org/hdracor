{-# LANGUAGE DeriveGeneric #-}
module Text.DraCor.Types
  where

import Data.Text (Text)
import GHC.Generics


-- * Records for storing json data from the DraCor API



type URL = Text

type Year = Text
type YearInt = Int

-- | 'Info' is a record for version information etc. about the API.
data Info = Info
  { apiName :: Text
  , apiStatus :: Text
  , apiExistdb :: Text
  , apiVersion :: Text
  } deriving (Generic, Show, Eq)


-- | A network node with node node-specific metrics.
data Node = Node
  { nodeId :: Text              -- ^ the ID of the node, must be given.
  , nodeWeightedDegree :: Maybe Int
  , nodeDegree :: Maybe Int
  , nodeCloseness :: Maybe Float
  , nodeEigenvector :: Maybe Float
  , nodeBetweenness :: Maybe Float
  } deriving (Show, Eq, Generic)


-- | A scene of a play and its speakers or persons on stage.
data Scene = Scene
  { scnNumber :: Int            -- ^ the running number of the scene, must be given
  , scnTitle :: Maybe Text
  , scnSpeakers :: [Text]
  , scnType :: Maybe Text
  } deriving (Generic, Show, Eq)


data Source
  = Source
  { srcName :: Maybe Text
  , srcUrl :: Maybe URL
  } 
  | SimpleSource { srcSource :: Text }
  deriving (Generic, Show, Eq)


data CastItem = CastItem
  { cstiId :: Maybe Text              -- ^ The ID used in the @who attribute of TEI
  , cstiName :: Maybe Text
  , cstiSex :: Maybe Text
  , cstiIsGroup :: Maybe Bool
  } deriving (Generic, Show, Eq)


data Author = Author
  { authrName :: Maybe Text
  , authrKey :: Maybe Text
  } deriving (Generic, Show, Eq)


data Metadata = Metadata
  { mtdId :: Text
  , mtdName :: Text
  , mtdCorpus :: Maybe Text
  -- , mtdPlayName :: Maybe Text -- redundant
  , mtdGenre :: Maybe Text
  , mtdTitle :: Maybe Text
  , mtdSubtitle :: Maybe Text
  , mtdAuthors :: [Author]
  -- , mtdAuthor :: Maybe Author   -- ^ deprecated
  , mtdSource :: Maybe Text
  , mtdSourceUrl :: Maybe URL
  , mtdOriginalSource :: Maybe Text
  , mtdYearPremiered :: Maybe YearInt
  , mtdYearPrinted :: Maybe YearInt
  , mtdYearNormalized :: Maybe YearInt
  , mtdYearWritten :: Maybe YearInt
  , mtdWikidataId :: Maybe Text
  , mtdNetworkdataCsvUrl :: Maybe URL
  -- -- deprecated meta data, see issue #83 of dracor-api
  -- , mtdPrintYear :: Maybe Text
  -- , mtdPremiereYear :: Maybe Text
  -- , mtdWrittenYear :: Maybe Text
  } deriving (Generic, Show, Eq)


data Metrics = Metrics
  { mtrSize :: Maybe Int
  , mtrAverageClustering :: Maybe Float
  , mtrNumOfPersonGroups :: Maybe Int
  , mtrDensity :: Maybe Float
  , mtrAveragePathLength :: Maybe Float
  , mtrMaxDegreeIds :: Maybe [Text]  -- Maybe [Text]
  , mtrAverageDegree :: Maybe Float
  , mtrDiameter :: Maybe Int
  , mtrMaxDegree :: Maybe Int
  , mtrNumOfSpeakers :: Maybe Int
  , mtrNumConnectedComponents :: Maybe Int
  , mtrNumOfSpeakersFemale :: Maybe Int
  , mtrNumOfSpeakersMale :: Maybe Int
  , mtrNumOfSpeakersUnknown :: Maybe Int
  , mtrNumOfSegments :: Maybe Int
  , mtrNumOfActs :: Maybe Int
  , mtrNetworkSize :: Maybe Int
  , mtrAllInIndex :: Maybe Float
  , mtrAllInSegment :: Maybe Int
  } deriving (Show, Eq, Generic)


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
  , plySource :: Maybe Text
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



data Corpus = Corpus
  { crpsName :: Maybe Text
  , crpsTitle :: Maybe Text
  , crpsRepository :: Maybe Text
  , crpsUri :: Maybe Text
  , crpsDramas :: Maybe [PlayFromCorpusList]  -- FIXME: should be Maybe [Play]
  } deriving (Generic, Show, Eq)



