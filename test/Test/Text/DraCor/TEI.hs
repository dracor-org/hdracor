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


test_parseTEI = do
  let exampleDir = "test/examples/"
      gerdracor = "../gerdracor/tei/"
      suff = "alberti-im-suff.xml"
      goetz = "goethe-goetz-von-berlichingen-mit-der-eisernen-hand.xml"
  xml <- B.readFile $ exampleDir ++ suff
  let result = parseTEI xml
  assertRight result
  assertEqual (Right 4) (fmap length result) -- 4 scenes
  assertEqual (Right 376) (fmap ((foldl (+) 0) . (map length)) result) -- 376 turn takings
  assertEqual (Right  [199, 17, 156, 4]) (fmap (map length) result) -- turn takings per scene

fetch :: String -> IO L.ByteString
fetch path = simpleHttp $ "https://dracor.org/api" ++ path

test_getAndParseTEI = do
  let corpus = "ger"
      suff = "alberti-im-suff"
  xml <- getTEI fetch corpus suff
  let result = parseTEI $ L.toStrict xml
  assertRight result
