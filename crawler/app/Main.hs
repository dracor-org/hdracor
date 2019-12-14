module Main where

import Options.Applicative
import Network.HTTP.Conduit
import Conduit (runConduit)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Control.Concurrent

import Text.DraCor.Types
import Text.DraCor.API


data Opts = Opts
  { url :: String
  , entry :: Entry
  , verbosity :: Verbosity
  }

data Entry
  = Corpora
  | SingleCorpus String
  | SinglePlay String String

data Verbosity
  = Normal
  | Verbose


opts_ :: Parser Opts
opts_ = Opts
  <$> argument str (metavar "URL"
                    <> help "Base URL of dracor API."
                    <> showDefault
                    <> value "https://dracor.org/api")
  <*> ((flag Corpora Corpora
        (long "corpora"
         <> short 'a'
         <> help "Crawl all corpora and everything in it. (Default)"))
       <|>
       (SingleCorpus
        <$> strOption (long "corpus"
                       <> short 'c'
                       <> help "Crawl a single corpus and everything in it."
                       <> metavar "CORPUS"))
       <|>
       (SinglePlay
        <$> strOption (long "play"
                       <> short 'p'
                       <> help "Crawl a single play."
                       <> metavar "CORPUS")
        <*> argument str (metavar "PLAY"
                          <> help "The play's name.")))
  <*> ((flag Normal Normal
        (long "normal"
         <> short 'n'
         <> help "Normal verbosity (Default)."))
       <|>
       (flag' Verbose
        (long "verbose"
         <> short 'v'
         <> help "Verbose output.")))


main = execParser opts >>= run
  where opts = info (helper <*> opts_)
          (fullDesc
           <> progDesc
           "crawldracor crawls a dracor-api for application testing."
           <> header "crawldracor  - crawl the api of a dracor endpoint.")


fetch' :: String -> String -> IO B.ByteString
-- fetch url path = do
--   request <- parseRequest $ url ++ path
--   manager <- newManager tlsManagerSettings
--   return $ responseBody $ httpLbs request manager
fetch' url path = simpleHttp $ url ++ path

fetch :: Opts -> (String -> IO B.ByteString)
fetch opts = fetch' $ url opts

run :: Opts -> IO ()
run opts@(Opts _ Corpora _) = crawlCorpora opts
run opts@(Opts _ (SingleCorpus name) _) = crawlCorpus opts name
run opts@(Opts _ (SinglePlay corpusName playName) _) = crawlPlay opts corpusName playName

crawlCorpora :: Opts -> IO ()
crawlCorpora opts = do
  cs <- getCorpora $ fetch opts
  case cs of
    Nothing -> do
      print "Failed parsing \"/corpora\"."
    Just cs' -> do
      mapM_ (crawlCorpus opts) $ catMaybes $ map ((fmap T.unpack) . crpsName) cs'

-- | Recursive crawling into a corpus
crawlCorpus :: Opts -> String -> IO ()
crawlCorpus opts corpusName = do
  case verbosity opts of
    Normal -> return ()
    Verbose -> do
      print $ "GET /corpora/" ++ corpusName
      return ()
  crps <- getCorpus (fetch opts) corpusName
  case crps of
    Nothing -> do
      print $ "Failed parsing \"/corpora/"  ++ corpusName ++ "\"."
    Just c -> do
      print $ "Crawling \"/corpora/"  ++ corpusName ++ "\"."
      let plays = map (T.unpack . mtdName . plyMetadata) $ crpsDramas c
      mapM_ (crawlPlay opts corpusName) plays
  threadDelay 500000

crawlPlay :: Opts -> String -> String -> IO ()
crawlPlay opts corpusName playName = do
  case verbosity opts of
    Normal -> return ()
    Verbose -> do
      print $ "GET /corpora/" ++ corpusName ++ "/play/" ++ playName
      return ()
  ply <- getPlay (fetch opts) corpusName playName
  case ply of
    Nothing -> do
      print $ "Failed parsing \"corpora/" ++ corpusName ++ "/play/" ++ playName ++ "\"."
    Just _ -> return ()
  ply <- getPlayMetrics (fetch opts) corpusName playName
  case ply of
    Nothing -> do
      print $ "Failed parsing \"corpora/" ++ corpusName ++ "/play/" ++ playName ++ "/metrics\"."
    Just _ -> return ()
  threadDelay 500000    
