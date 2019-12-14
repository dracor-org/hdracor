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
import Text.DraCor.Utils


-- * JSON for 'Source'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Source)


-- * JSON for 'Node'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 4} ''Node)


-- -- * JSON for 'Play'

-- $(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Play)


-- -- * JSON for 'PlayFromCorpusList'

-- $(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 5} ''PlayFromCorpusList)


-- -- * JSON for 'Corpus'

-- $(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 4} ''Corpus)
