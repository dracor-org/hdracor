{-# LANGUAGE TemplateHaskell #-}
module Text.DraCor.IntolerantJSON
  where

-- | This module defines a set of parsers and serializers for JSON
-- without workarounds for issues in the dracor API.

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Aeson.TH as ATH

import Text.DraCor.Types
import Text.DraCor.CommonJSON
