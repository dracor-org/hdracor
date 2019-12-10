{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Text.DraCor.Types
  where

import Test.Framework
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Text.DraCor.Types

test_info = do
  s <- B.readFile "test/examples/info"
  assertEqual True (isJust (decode s :: Maybe API))
  assertEqual (Just "DraCor API")  (fmap apiName $ (decode s :: Maybe API))
  
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


