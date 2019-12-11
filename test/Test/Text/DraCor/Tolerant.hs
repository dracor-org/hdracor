{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Text.DraCor.Tolerant
  where

import Test.Framework
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Monad

import Text.DraCor.Types
import Text.DraCor.TolerantJSON

-- * low level: logical parts of data returned by the api

test_scene = do
  s <- B.readFile "test/examples/scene1.json"
  assertEqual True (isJust (decode s :: Maybe Scene))
  assertEqual
    (Just ["dietrich", "helldrungen", "erich"])
    (fmap scnSpeakers (decode s :: Maybe Scene))
  
test_sceneNoSpeakers = do
  s <- B.readFile "test/examples/scene2.json"
  assertEqual True (isJust (decode s :: Maybe Scene))


test_metadata = do
  s <- B.readFile "test/examples/metadata.json"
  assertEqual True (isJust (decode s :: Maybe Metadata))
  assertEqual (Just [ Author
                      { authrName = Just "Alberti, Konrad"
                      , authrKey = Just "pnd:116009926"
                      } ])
    (fmap mtdAuthors (decode s :: Maybe Metadata))
  assertEqual (Just "TextGrid Repository")
    (join $ fmap mtdSource (decode s :: Maybe Metadata))

test_metadata2 = do
  s <- B.readFile "test/examples/metadata2.json"
  assertEqual True (isJust (decode s :: Maybe Metadata))
  assertEqual (Just [ Author
                      { authrName = Just "Alberti, Konrad"
                      , authrKey = Just "pnd:116009926"
                      } ])
    (fmap mtdAuthors (decode s :: Maybe Metadata))

test_metadata3 = do
  s <- B.readFile "test/examples/metadata3.json"
  assertEqual True (isJust (decode s :: Maybe Metadata))
  assertEqual (Just [ Author
                      { authrName = Just "Alberti, Konrad"
                      , authrKey = Just "pnd:116009926"
                      } ])
    (fmap mtdAuthors (decode s :: Maybe Metadata))
  assertEqual (Just "TextGrid Repository")
    (join $ fmap mtdSource (decode s :: Maybe Metadata))


-- * high level: data presented by the api

test_info = do
  s <- B.readFile "test/examples/info"
  assertEqual True (isJust (decode s :: Maybe Info))
  assertEqual (Just "DraCor API")  (fmap apiName $ (decode s :: Maybe Info))
  
test_corpora = do
  s <- B.readFile "test/examples/corpora"
  assertEqual True (isJust (decode s :: Maybe [Corpus]))

test_corporaGer = do
  s <- B.readFile "test/examples/corpora-ger"
  assertEqual True (isJust (decode s :: Maybe Corpus))

-- see issue #85 of dracor-api
test_corporaGerMetadata = do
  s <- B.readFile "test/examples/corpora-ger-metadata"
  assertEqual True (isJust (decode s :: Maybe [Play]))

test_corporaGerPlayAlbertiBrot = do
  s <- B.readFile "test/examples/corpora-ger-play-alberti-brot"
  assertEqual True (isJust (decode s :: Maybe Play))

-- see issue #87 of the dracor-api
test_corporaGerPlayAlbertiBrotMetrics = do
  s <- B.readFile "test/examples/corpora-ger-play-alberti-brot-metrics"
  assertEqual True (isJust (decode s :: Maybe Play))


