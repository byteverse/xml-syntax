{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Xml

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import qualified Data.Bytes.Text.Ascii as Ascii

main :: IO ()
main = defaultMain tests

decodeString :: String -> Maybe Node
decodeString = Xml.decode . Ascii.fromString

tests :: TestTree
tests =
  testGroup
    "xml"
    [ testCase "A" $
        Just
          ( Element
              Content
                { tag = "foo"
                , attributes = mempty
                , children = [Text "hello"]
                }
          )
          @=? decodeString "<foo>hello</foo>"
    , testCase "B" $
        Just
          ( Element
              Content
                { tag = "foo"
                , attributes = [Attribute "bar" "baz"]
                , children = [Text "hi"]
                }
          )
          @=? decodeString "<foo bar='baz'>hi</foo>"
    , testCase "C" $
        Just
          ( Element
              Content
                { tag = "foo"
                , attributes = [Attribute "bar" "baz"]
                , children = [Text "hi"]
                }
          )
          @=? decodeString "<foo bar=\"baz\">hi</foo>"
    , testCase "D" $
        Just
          ( Element
              Content
                { tag = "foo"
                , attributes = [Attribute "bar" "baz"]
                , children = mempty
                }
          )
          @=? decodeString "<foo bar=\"baz\"/>"
    , testCase "E" $
        Just
          ( Element
              Content
                { tag = "foo"
                , attributes = mempty
                , children = [Text "hello"]
                }
          )
          @=? decodeString "<foo   >hello</foo>"
    , testCase "F" $
        Just
          ( Element
              Content
                { tag = "foo"
                , attributes = [Attribute "bar" "baz"]
                , children =
                    [ Text "  "
                    , Element Content {tag = "hey", attributes = mempty, children = mempty}
                    , Text " "
                    , Element Content {tag = "hi", attributes = mempty, children = mempty}
                    ]
                }
          )
          @=? decodeString "<foo bar=\"baz\">  <hey/> <hi></hi></foo>"
    ]
