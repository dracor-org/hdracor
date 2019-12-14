module Main where

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


fetch :: String -> String -> IO B.ByteString
-- fetch url path = do
--   request <- parseRequest $ url ++ path
--   manager <- newManager tlsManagerSettings
--   return $ responseBody $ httpLbs request manager
fetch url path = simpleHttp $ url ++ path


fetchDraCor = fetch "https://dracor.org/api"

main :: IO ()
main = do
  crawlCorpus "ger"
  return ()

crawlCorpora :: IO ()
crawlCorpora = do
  cs <- getCorpora fetchDraCor
  case cs of
    Nothing -> do
      print "Failed parsing \"/corpora\"."
    Just cs' -> do
      mapM_ crawlCorpus $ catMaybes $ map ((fmap T.unpack) . crpsName) cs'

-- | Recursive crawling into a corpus
crawlCorpus :: String -> IO ()
crawlCorpus corpusName = do
  crps <- getCorpus fetchDraCor corpusName
  case crps of
    Nothing -> do
      print $ "Failed parsing \"/corpora/"  ++ corpusName ++ "\"."
    Just c -> do
      print $ "Crawling \"/corpora/"  ++ corpusName ++ "\"."
      let plays = map (T.unpack . mtdName . plyMetadata) $ crpsDramas c
      mapM_ (crawlPlay corpusName) plays
  threadDelay 500000

crawlPlay :: String -> String -> IO ()
crawlPlay corpusName playName = do
  ply <- getPlay fetchDraCor corpusName playName
  case ply of
    Nothing -> do
      print $ "Failed parsing \"corpora/" ++ corpusName ++ "/play/" ++ playName ++ "\"."
    Just _ -> return ()
  ply <- getPlayMetrics fetchDraCor corpusName playName
  case ply of
    Nothing -> do
      print $ "Failed parsing \"corpora/" ++ corpusName ++ "/play/" ++ playName ++ "/metrics\"."
    Just _ -> return ()
  threadDelay 500000    
