{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} Test.Text.DraCor.Tolerant
--import {-@ HTF_TESTS @-} Test.Text.DraCor.Intolerant
import {-@ HTF_TESTS @-} Test.Text.DraCor.TEI

main = htfMain htf_importedTests
