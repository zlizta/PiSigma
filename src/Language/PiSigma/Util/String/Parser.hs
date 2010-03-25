{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.PiSigma.Util.String.Parser
  ( String
  , append
  , fromString
  , isPrefixOf
  , lines
  , null
  , readFile
  , span
  , toString
  , uncons
  , unlines )
  where

import Prelude
  hiding
    ( String
    , lines
    , null
    , readFile
    , replicate
    , span
    , unlines )
import qualified Prelude
  as Prelude
import qualified Data.ByteString.Lazy
  as Lazy
import qualified Data.ByteString.Lazy.UTF8
  as LazyUTF8
import qualified Data.String as String

-- * The parser input type

type String = LazyUTF8.ByteString

instance String.IsString String where
  fromString = fromString

append     :: String -> String -> String
append      = Lazy.append

fromString :: Prelude.String -> String
fromString  = LazyUTF8.fromString

isPrefixOf :: String -> String -> Bool
isPrefixOf  = Lazy.isPrefixOf

lines      :: String -> [String]
lines       = LazyUTF8.lines

null       :: String -> Bool
null        = Lazy.null

readFile   :: FilePath -> IO String
readFile    = Lazy.readFile

span       :: (Char -> Bool) -> String -> (String, String)
span        = LazyUTF8.span

toString   :: String -> Prelude.String
toString    = LazyUTF8.toString

uncons     :: String -> Maybe (Char, String)
uncons      = LazyUTF8.uncons

unlines    :: [String] -> String
unlines     = Lazy.intercalate "\n"



-- NOTE: Commenting out the code above and uncommenting the code below
-- results in an incredible slowdown during parsing.  I'm not sure why
-- this is since Data.Text should be pretty fast.  Using strict versus
-- lazy Data.Text is even worse.

-- import Control.Monad
--   ( liftM )
-- import qualified Data.ByteString.Lazy
--   as Word8
-- import qualified Data.Text.Lazy
--   as Text
-- import qualified Data.Text.Lazy.Encoding
--   as Text

-- type String = Text.Text

-- append     :: String -> String -> String
-- append      = Text.append

-- fromString :: Prelude.String -> String
-- fromString  = Text.pack

-- isPrefixOf :: String -> String -> Bool
-- isPrefixOf  = Text.isPrefixOf

-- lines      :: String -> [String]
-- lines       = Text.lines

-- null       :: String -> Bool
-- null        = Text.null

-- readFile   :: FilePath -> IO String
-- readFile    = liftM Text.decodeUtf8 . Word8.readFile

-- span       :: (Char -> Bool) -> String -> (String, String)
-- span        = Text.spanBy

-- toString   :: String -> Prelude.String
-- toString    = Text.unpack

-- uncons     :: String -> Maybe (Char, String)
-- uncons      = Text.uncons

-- unlines    :: [String] -> String
-- unlines     = Text.unlines
