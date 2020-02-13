{-# LANGUAGE DeriveGeneric #-}
module Text.DraCor.Types
  where

import Data.Text (Text)
import GHC.Generics


-- * Records for storing json data from the DraCor API

type URL = Text

type Year = Int

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
  , nodeCloseness :: Maybe Double
  , nodeEigenvector :: Maybe Double
  , nodeBetweenness :: Maybe Double
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

sourceName :: Source -> Maybe Text
sourceName (Source n _) = n
sourceName (SimpleSource n) = Just n

sourceUrl :: Source -> Maybe URL
sourceUrl (Source _ u) = u
sourceUrl (SimpleSource _) = Nothing


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
  -- , mtdPlayName :: Maybe Text -- is redundant
  , mtdGenre :: Maybe Text
  , mtdTitle :: Maybe Text
  , mtdSubtitle :: Maybe Text
  , mtdAuthors :: [Author]
  , mtdSource :: Maybe Text
  , mtdSourceUrl :: Maybe URL
  , mtdOriginalSource :: Maybe Text -- What's the difference to mdtSource?
  , mtdYearPremiered :: Maybe Year
  , mtdYearPrinted :: Maybe Year
  , mtdYearNormalized :: Maybe Year
  , mtdYearWritten :: Maybe Year
  , mtdWikidataId :: Maybe Text
  , mtdNetworkdataCsvUrl :: Maybe URL
  } deriving (Generic, Show, Eq)


-- | A record of a play's metrics.
data Metrics = Metrics
  { -- name, corpus and ID: Isn't it more haskell'isch to have it outside?
    mtrSize :: Maybe Int
  , mtrAverageClustering :: Maybe Double
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
    -- Nodes
  , mtrNodes :: [Node]
  } deriving (Show, Eq, Generic)


-- * Text, spoken aloud and mute

-- | Wrappers for the types of text in a play. The wrappers are
-- realized by type constructors.
data TextState
  = Aloud Text                -- ^ text spoken by some character
  | Stage Text                -- ^ stage direction
  | Speaker Text              -- ^ speaker name
  | Mute Text                 -- ^ other mute text, i.e. text not spoken aloud
  deriving (Show, Eq)

-- | Unpack the 'Text' from a stateful 'TextState' text.
getTextStateText :: TextState -> Text
getTextStateText (Mute txt) = txt
getTextStateText (Aloud txt) = txt
getTextStateText (Stage txt) = txt
getTextStateText (Speaker txt) = txt

-- | A predicate that is 'True', if the 'TextState' of given text is
-- 'Speaker'. Useful for filtering.
isSpeaker :: TextState -> Bool
isSpeaker (Speaker _) = True
isSpeaker _ = False

-- | A predicate that is 'True', if the 'TextState' of given text is
-- 'Speach' of 'Stage'.
isSpeachOrStage :: TextState -> Bool
isSpeachOrStage (Aloud _) = True
isSpeachOrStage (Stage _) = True
isSpeachOrStage _ = False

-- | A speach (turn taking) of a theater play, with a (one or many)
-- speaker and a list of stateful speach texts.
data Speach = Speach
  { speachWho :: [Text]
  , speachSpeaker :: [Text]
  , speachSpeach :: [TextState]
  } deriving (Show, Eq)

-- * Play

-- | A record of a play with 'Metadata', 'Metrics', and lists of
-- 'Scene', 'CastItem'.
data Play = Play
  { -- meta data
    plyMetadata :: Metadata
    -- Metrics
  , plyMetrics :: Metrics
    -- Scenes
  , plySegments :: [Scene]
    -- characters
  , plyCast :: [CastItem]
  } deriving (Generic, Show, Eq)

-- * Corpus

data Corpus = Corpus
  { crpsName :: Maybe Text
  , crpsTitle :: Maybe Text
  , crpsRepository :: Maybe Text
  , crpsUri :: Maybe Text
  , crpsDramas :: [Play]
  } deriving (Generic, Show, Eq)
