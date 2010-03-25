module Language.PiSigma.Util.String.Internal
  ( String
  , append
  , concat
  , fromString
  , isPrefixOf
  , null
  , putStrLn
  , toString )
  where

import Prelude
  hiding
    ( String
    , concat
    , null
    , putStrLn )
import qualified Prelude
  as Prelude
import qualified Data.ByteString
  as Word8
import qualified Data.Text
  as Text
import qualified Data.Text.Encoding
  as Text

type String = Text.Text

append     :: String -> String -> String
append      = Text.append

concat     :: [String] -> String
concat      = Text.concat

fromString :: Prelude.String -> String
fromString  = Text.pack

isPrefixOf :: String -> String -> Bool
isPrefixOf  = Text.isPrefixOf

null       :: String -> Bool
null        = Text.null

putStrLn   :: String -> IO ()
putStrLn    = Word8.putStrLn . Text.encodeUtf8

toString   :: String -> Prelude.String
toString    = Text.unpack
