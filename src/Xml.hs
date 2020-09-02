{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}

module Xml
  ( Node(..)
  , Content(..)
  , Attribute(..)
  , decode
  ) where

import Data.Word (Word8)
import Data.Bytes (Bytes)
import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray)
import Data.Bytes.Parser (Parser)
import GHC.Exts (Char(C#),Char#)
import Data.Chunks (Chunks)
import Data.Builder.ST (Builder)

import qualified Data.Chunks as Chunks
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified Data.Builder.ST as Builder
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Rebindable as R
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Bytes.Parser.Utf8 as Utf8

data Node
  = Text !ShortText
  | Element {-# UNPACK #-} !Content
  deriving (Show,Eq)

data Content = Content
  { tag :: !ShortText
  , attributes :: !(SmallArray Attribute)
  , children :: !(SmallArray Node)
  } deriving (Show,Eq)

data Attribute = Attribute
  { name :: !ShortText
  , value :: !ShortText
  } deriving (Show,Eq)

decode :: Bytes -> Maybe Node
decode !b = Parser.parseBytesMaybe elementNodeParser b

elementNodeParser :: Parser () s Node
elementNodeParser = do
  Latin.char () '<'
  btag <- Parser.takeWhile (\w -> not (isXmlSpace w) && w /= 0x3E && w /= 0x2F)
  case Bytes.length btag of
    0 -> Parser.fail ()
    _ -> pure ()
  tag <- case TS.fromShortByteString (Bytes.toShortByteStringClone btag) of
    Nothing -> Parser.fail ()
    Just ttag -> pure ttag
  -- Note that parserAttributes consumes leading and trailing whitespace.
  attrs <- parserAttributes =<< Parser.effect Builder.new
  let !attributes = Chunks.concat attrs
  Latin.any () >>= \case
    '>' -> do
      nodes <- childrenParser tag
      pure (Element Content{tag,attributes,children=Chunks.concat nodes})
    '/' -> do
      Latin.char () '>'
      pure (Element Content{tag,attributes,children=mempty})
    _ -> Parser.fail ()
      
textNodeParser :: Parser () s Node
textNodeParser = do
  raw <- Parser.takeWhile (\w -> w /= 0x3C)
  case Bytes.any (\w -> w > 0x7F || w == 0x26) raw of
    True -> Parser.fail () -- TODO: escape or check UTF-8 encoding here instead
    False -> pure (Text (TS.fromShortByteStringUnsafe (Bytes.toShortByteStringClone raw)))

-- This eats the closing tag as well.
childrenParser ::
     ShortText -- opening tag name, looking for a closing tag that matches
  -> Parser () s (Chunks Node)
childrenParser !tag = do
  b0 <- Parser.effect Builder.new
  childrenParserLoop tag b0

childrenParserLoop ::
     ShortText -- opening tag name, looking for a closing tag that matches
  -> Builder s Node
  -> Parser () s (Chunks Node)
childrenParserLoop !tag !b0 = Latin.any () >>= \case
  '<' -> Latin.any () >>= \case
    '/' -> do
      Utf8.shortText () tag
      Parser.skipWhile isXmlSpace
      Latin.char () '>'
      Parser.effect (Builder.freeze b0)
    _ -> do
      Unsafe.unconsume 2
      node <- elementNodeParser
      b1 <- Parser.effect (Builder.push node b0)
      childrenParserLoop tag b1
  _ -> do
    Unsafe.unconsume 1
    node <- textNodeParser
    b1 <- Parser.effect (Builder.push node b0)
    childrenParserLoop tag b1

isXmlSpace :: Word8 -> Bool
isXmlSpace = \case
  0x20 -> True
  0x09 -> True
  0x0D -> True
  0x0A -> True
  _ -> False

parserAttributes :: Builder s Attribute -> Parser () s (Chunks Attribute)
parserAttributes !b0 = do
  Parser.skipWhile isXmlSpace
  peekIsNameStartChar >>= \case
    True -> do
      attr <- parserAttribute
      b1 <- Parser.effect (Builder.push attr b0)
      parserAttributes b1
    False -> do
      Parser.skipWhile isXmlSpace
      Parser.effect (Builder.freeze b0)

-- From the spec, we have:
--   Attribute ::= Name Eq AttValue
--   Eq        ::= S? '=' S?
--   Name      ::= NameStartChar (NameChar)*
--
-- Precondition A: The first character is a NameStartChar. This parser
-- does not check this.
parserAttribute :: Parser () s Attribute
parserAttribute = do
  bname <- Parser.takeWhile (\w -> not (isXmlSpace w) && w /= 0x3D)
  -- We may assume that length of bname is at least one because of
  -- precondition A.
  !name <- case TS.fromShortByteString (Bytes.toShortByteStringClone bname) of
    Nothing -> Parser.fail ()
    Just tname -> pure tname
  Parser.skipWhile isXmlSpace
  Latin.char () '='
  Parser.skipWhile isXmlSpace
  !value <- parserAttributeValue
  pure Attribute{name,value}

-- TODO: This is woefully incomplete
parserAttributeValue :: Parser () s ShortText
parserAttributeValue = do
  Latin.any () >>= \case
    '"' -> do
      bval <- Parser.takeWhile (\w -> w /= 0x22)
      Latin.char () '"'
      case TS.fromShortByteString (Bytes.toShortByteStringClone bval) of
        Nothing -> Parser.fail ()
        Just tval -> pure tval
    '\'' -> do
      bval <- Parser.takeWhile (\w -> w /= 0x27)
      Latin.char () '\''
      case TS.fromShortByteString (Bytes.toShortByteStringClone bval) of
        Nothing -> Parser.fail ()
        Just tval -> pure tval
    _ -> Parser.fail ()
  
peekIsNameStartChar :: Parser () s Bool
peekIsNameStartChar =
  Unsafe.cursor R.>>= \pos ->
  Utf8.any# () R.>>= \c -> 
  Unsafe.jump pos R.>>= \_ ->
  R.pure (isNameStartChar c)
  
isNameStartChar :: Char# -> Bool
isNameStartChar c = case C# c of
  ':' -> True
  '_' -> True
  _ | C# c >= 'A' && C# c <= 'Z' -> True
  _ | C# c >= 'a' && C# c <= 'z' -> True
  _ -> False
