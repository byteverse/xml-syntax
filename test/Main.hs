{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Xml

import Data.Bytes.Chunks (Chunks(ChunksNil,ChunksCons))
import Data.Proxy (Proxy(..))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),testCase)

import qualified Xml
import qualified Data.Bytes as Bytes
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU

main :: IO ()
main = defaultMain tests

decodeString :: String -> Maybe Node
decodeString = Xml.decode . Bytes.fromAsciiString

tests :: TestTree
tests = testGroup "xml"
  [ testCase "A" $
      Just
        ( Element Content
          { tag = "foo"
          , attributes = mempty
          , children = [Text "hello"]
          }
        )
      @=?
      decodeString "<foo>hello</foo>"
  , testCase "B" $
      Just
        ( Element Content
          { tag = "foo"
          , attributes = [Attribute "bar" "baz"]
          , children = [Text "hi"]
          }
        )
      @=?
      decodeString "<foo bar='baz'>hi</foo>"
  , testCase "C" $
      Just
        ( Element Content
          { tag = "foo"
          , attributes = [Attribute "bar" "baz"]
          , children = [Text "hi"]
          }
        )
      @=?
      decodeString "<foo bar=\"baz\">hi</foo>"
  , testCase "D" $
      Just
        ( Element Content
          { tag = "foo"
          , attributes = [Attribute "bar" "baz"]
          , children = mempty
          }
        )
      @=?
      decodeString "<foo bar=\"baz\"/>"
  , testCase "E" $
      Just
        ( Element Content
          { tag = "foo"
          , attributes = mempty
          , children = [Text "hello"]
          }
        )
      @=?
      decodeString "<foo   >hello</foo>"
  , testCase "F" $
      Just
        ( Element Content
          { tag = "foo"
          , attributes = [Attribute "bar" "baz"]
          , children =
            [ Text "  "
            , Element Content{tag="hey",attributes=mempty,children=mempty}
            , Text " "
            , Element Content{tag="hi",attributes=mempty,children=mempty}
            ]
          }
        )
      @=?
      decodeString "<foo bar=\"baz\">  <hey/> <hi></hi></foo>"
  ]
