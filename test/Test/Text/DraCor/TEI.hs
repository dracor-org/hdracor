{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Text.DraCor.TEI
  where

import Test.Framework
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Either
import Network.HTTP.Conduit

import Text.DraCor.Types
import Text.DraCor.TEI
import Text.DraCor.API


exampleDir = "test/examples/"
gerdracor = "../gerdracor/tei/"

suff = "alberti-im-suff"
goetz = "goethe-goetz-von-berlichingen-mit-der-eisernen-hand"
palindrom = "FalknerPalindrom"

test_parseTEIsuff = do
  xml <- B.readFile $ exampleDir ++ suff ++ ".xml"
  let result = parseTEI xml
  assertRight result
  assertEqual (Right 4) (fmap length result) -- 4 scenes
  assertEqual (Right 376) (fmap ((foldl (+) 0) . (map length)) result) -- 376 turn takings
  assertEqual (Right  [199, 17, 156, 4]) (fmap (map length) result) -- turn takings per scene

test_parseTEIpalindrom = do
  -- unitTestPending "Get this right!"
  xml <- B.readFile $ exampleDir ++ palindrom ++ ".TEI-P5.xml"
  let result = parseTEI xml
  assertRight result
  assertEqual (Right 1) (fmap length result) -- 1 scenes
  assertEqual (Right 29) (fmap ((foldl (+) 0) . (map length)) result) -- 29 turn takings
  assertEqual (Right  [29]) (fmap (map length) result) -- turn takings per scene

test_parseTEIleftOnUnmatchingClosingTag = do
  xml <- B.readFile $ exampleDir ++ palindrom ++ ".TEI-P5.xml.broken1"
  let result = parseTEI xml
  assertLeft result

test_parseTEIleftOnTooManyClosingTags = do
  xml <- B.readFile $ exampleDir ++ palindrom ++ ".TEI-P5.xml.broken2"
  let result = parseTEI xml
  assertLeft result


fetch :: String -> IO L.ByteString
fetch path = simpleHttp $ "https://dracor.org/api" ++ path

test_getAndParseTEI = do
  let corpus = "ger"
      suff = "alberti-im-suff"
  xml <- getTEI fetch corpus suff
  let result = parseTEI $ L.toStrict xml
  assertRight result
