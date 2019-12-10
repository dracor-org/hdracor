module Text.DraCor.Utils
  where

import Data.Char


-- | Function to transforming record field names to JSON.
lowerInit :: String -> String
lowerInit (x:xs) = (toLower x):xs

modifyField :: Int -> String -> String
modifyField i s = lowerInit $ drop i s
