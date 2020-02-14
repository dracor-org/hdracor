{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Text.DraCor.TEI
  ( parseTEI
  ) where

-- | This module provides a SAX parser for TEI encoded theater plays.

import Xeno.SAX
import Xeno.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Lens
import Text.Regex.TDFA
import Data.Char

import Text.DraCor.Types

-- * Internals of the SAX parser

data SaxState = SaxState
  { _tagStack :: [B.ByteString] -- ^ a stack of open tags
  , _inBody :: Bool             -- ^ state: whether we are inside the body element
  , _inSp :: Bool               -- ^ state: whether we are inside an sp element
  , _textState :: [(T.Text -> TextState)] -- ^ a stack of type constructors
  , _scenes :: [[Speach]]  -- ^ stack of text found in scenes
  , _who :: T.Text         -- ^ for storing the who attribute
  , _speaker :: [T.Text]   -- ^ for storing the speakers of a text
  , _speach :: [TextState] -- ^ for storing parsed text
  }

makeLenses ''SaxState

initialSaxState :: Either String SaxState
initialSaxState = Right $ SaxState
  { _tagStack = []
  , _inBody = False
  , _inSp = False
  , _textState = [Mute]
  , _scenes = [[]]
  , _who = ""
  , _speaker = []
  , _speach = []
  }

prefix :: String
prefix = "([a-zA-Z][[:alnum:]]?:)?"

-- FIXME: Add real handling of namespaces!
teiTag :: SaxState -> String -> String
teiTag s localName = prefix ++ localName ++ "$"

mkSpeach :: SaxState -> Speach
mkSpeach s = Speach
  { speachWho = T.words $ _who s
  , speachSpeaker = filter ((>0) . T.length) $
                    map (T.strip . (T.dropAround isPunctuation) . T.strip . getTextStateText) $
                    filter isSpeaker $ _speach s
  , speachSpeach = filter isSpeachOrStage $ _speach s
  }

-- FIXME: Get encoding from SaxState
toText :: B.ByteString -> T.Text
toText = T.decodeUtf8

openTag :: Either String SaxState -> B.ByteString -> Either String SaxState
openTag (Left s) tag = Left s
openTag (Right s) tag
  -- entering body
  | (tag =~ (teiTag s "body") :: Bool)
  = Right $ s & tagStack %~ (tag:)
    & inBody .~ True
  -- a new scene
  | (tag =~ (teiTag s "div") :: Bool)
    && _inBody s
    -- We also check, if the last element in stack of speaches is an
    -- empty list, because for nested divs--like in plays with acts
    -- and scenes--the outer would produce an empty list. Downside: No
    -- scenes without speaches!
    && ((last $ _scenes s) /= [])
  = Right $ s & tagStack %~ (tag:)
    & scenes %~ (++[[]])
  -- opening sp
  | tag =~ (teiTag s "sp") :: Bool
  = Right $ s & tagStack %~ (tag:)
    & inSp .~ True
    & who .~ ""
    & speaker .~ []
    & speach .~ []
  -- opening speaker in sp
  | (tag =~ (teiTag s "speaker") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Speaker:)
  -- opening stage in sp
  | (tag =~ (teiTag s "stage") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Stage:)
  -- opening p in sp
  | (tag =~ (teiTag s "p") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Aloud:)
  -- opening p in lg
  | (tag =~ (teiTag s "lg") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Aloud:)
  --
  | otherwise
  = Right $ s & tagStack %~ (tag:)


attrNode :: Either String SaxState -> B.ByteString -> B.ByteString -> Either String SaxState
attrNode (Left err) _ _ = Left err
attrNode (Right s) k v
  | _inSp s
    && k == "who"
  = Right $ s & who .~ (toText v)
  | otherwise = Right s


closeTag :: Either String SaxState -> B.ByteString -> Either String SaxState
closeTag (Left err) _ = Left err
closeTag (Right s) tag
  | _tagStack s == []
  = Left "Fatal XML error: closing tag while no open tags left"
  | tag /= head (_tagStack s)
  = Left $ "Fatal XML error: unexpected closing tag " -- ++ (show $ B.unpack tag)
  -- closing sp
  | (tag =~ (teiTag s "sp") :: Bool)
    -- Why do we have to check, that there are data? Without, there
    -- will be an Speach element in the list from the initial SaxState
    -- in a play like FalknerPalindrom. But there is no closing xml sp
    -- element before the first div! FIXME or EXPLAIN
    && ((_who s /= "")
        || (_speaker s /= [])
        || ((filter isSpeachOrStage $ _speach s) /= []))
  = Right $ s & tagStack %~ tail
    & scenes %~ (\xs -> init xs ++ [(last xs) ++ [mkSpeach s]])
    & who .~ ""
    & speaker .~ []
    & speach .~ []
    & inSp .~ False
    & textState %~ tail
  -- closing stage in sp
  | (tag =~ (teiTag s "stage") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ tail
    & textState %~ tail
  -- closing p in sp
  | (tag =~ (teiTag s "p") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ tail
    & textState %~ tail
  -- closing lg in sp
  | (tag =~ (teiTag s "lg") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ tail
    & textState %~ tail
  --
  | otherwise
  = Right $ s & tagStack %~ tail

textNode :: Either String SaxState -> B.ByteString -> Either String SaxState
textNode (Left err) _ = Left err
textNode (Right s) str
  | _inSp s
  = Right $ s & speach %~ ((constructTxt s $ toText str):)
  | otherwise = Right s
  where
    constructTxt :: SaxState -> (T.Text -> TextState)
    constructTxt s
      -- FIXME: The first case is never true because of the initial
      -- sax state. Add tests to assert it. The guard slows things
      -- down.
      | (length $ _textState s) == 0 = Mute
      | otherwise = head $ _textState s

endOpenTag :: Either String SaxState -> B.ByteString -> Either String SaxState
endOpenTag s _ = s

cdata :: Either String SaxState -> B.ByteString -> Either String SaxState
cdata s _ = s


-- * The parser

-- | A parser for TEI encoded theater plays. A 'Left' value is
-- returned when a parser error has occurred. The speaches of the play
-- are wrapped into a 'Right' value.
--
-- The parser checks for properly nested elements.
parseTEI :: B.ByteString -> Either String [[Speach]]
parseTEI =
  fmap _scenes .
  joinEithers .
  fold openTag attrNode endOpenTag textNode closeTag cdata initialSaxState
  where
    joinEithers :: Either XenoException (Either String SaxState) -> Either String SaxState
    joinEithers (Left xerr) = Left $ show xerr
    joinEithers (Right result) = result


-- * Playing around

-- | Parse an example play with @stack runghc TEI.hs@
main :: IO ()
main = do
  let gerdracor = "../../../../gerdracor/tei/"
      exampleDir = "../../../test/examples/"
      suff = "alberti-im-suff.xml"
      palindrom = "FalknerPalindrom.TEI-P5.xml"
      goetz = "goethe-goetz-von-berlichingen-mit-der-eisernen-hand.xml"
  xml <- B.readFile $ exampleDir ++ palindrom
  let result = parseTEI xml
  case result of
    Left err -> do
      print err
    Right s -> do
      -- print s
      -- mapM_ (print . (map speachWho)) s
      -- print $ show $ s !! 5 --- $ map (map speachSpeaker) s
      print $ length $ s
      print $ map length s
      print $ foldl (+) 0 $ map length s
