{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Text.DraCor.CommonJSON where

-- | Common parsers an serializers used by the tolerant and intolerant
-- library.

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Aeson.TH as ATH

import Text.DraCor.Types
import Text.DraCor.Utils


-- * JSON parser for 'Info'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Info)


-- * JSON for 'Source'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Source)


-- * JSON for 'Node'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 4} ''Node)


-- * JSON for 'Scene'

--  $(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Scene)

instance FromJSON Scene where
  parseJSON = withObject "Scene" $ \s -> Scene
    <$> s .: "number"
    <*> (fmap (fmap T.pack) $ s .:? "title")
    <*> (s .:? "speakers" .!= [])
    <*> (fmap (fmap T.pack) $ s .:? "type")

instance ToJSON Scene


-- * JSON for 'CastItem'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 4} ''CastItem)


-- * JSON for 'Author'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 5} ''Author)


-- * JSON for 'Play'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 3} ''Play)


-- * JSON for 'PlayFromCorpusList'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 5} ''PlayFromCorpusList)


-- * JSON for 'Corpus'

$(ATH.deriveJSON ATH.defaultOptions{ATH.fieldLabelModifier = modifyField 4} ''Corpus)

