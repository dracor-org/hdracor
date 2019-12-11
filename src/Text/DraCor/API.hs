module Text.DraCor.API
  where

-- | Wrappers around a DraCor API

import qualified Data.ByteString.Lazy as B
import Data.Aeson

import Text.DraCor.Types
import Text.DraCor.TolerantJSON

-- * Public

getInfo :: (String -> IO B.ByteString) -> IO (Maybe Info)
getInfo f = do
  s <- f "/info"
  return $ (decode s :: Maybe Info)

getCorpora :: (String -> IO B.ByteString) -> IO (Maybe [Corpus])
getCorpora f = do
  s <- f "/corpora"
  return $ (decode s :: Maybe [Corpus])

getCorpus :: (String -> IO B.ByteString) -> String -> IO (Maybe Corpus)
getCorpus f corpus = do
  s <- f $ "/corpora/" ++ corpus
  return $ (decode s :: Maybe Corpus)

getCorpusMetadata :: (String -> IO B.ByteString) -> String -> IO (Maybe [Play])
getCorpusMetadata f corpus = do
  s <- f $ "/corpora/" ++ corpus ++ "/metadata"
  return $ (decode s :: Maybe [Play])

getPlay :: (String -> IO B.ByteString) -> String -> String -> IO (Maybe Play)
getPlay f corpus play = do
  s <- f $ "/corpora/" ++ corpus ++ "/play/" ++ play
  return $ (decode s :: Maybe Play)

getPlayMetrics :: (String -> IO B.ByteString) -> String -> String -> IO (Maybe Play)
getPlayMetrics f corpus play = do
  s <- f $ "/corpora/" ++ corpus ++ "/play/" ++ play ++ "/metrics"
  return $ (decode s :: Maybe Play)

