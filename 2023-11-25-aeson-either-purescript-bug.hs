-- https://github.com/purescript-contrib/purescript-argonaut-codecs/issues/115

{-# LANGUAGE DerivingVia, DeriveAnyClass, DeriveGeneric, UndecidableInstances #-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString as ByteString
import qualified Prelude

import Data.Aeson (GToJSON, GToJSON', Encoding, Value, One, Zero, ToJSON)
import GHC.Generics (Generic, Rep)
import Prelude hiding (Either(..))

data Either e a = Left e | Right a
  deriving Generic
  deriving ToJSON

newtype Foo = Foo { fooEither :: Prelude.Either () String }
  deriving Generic

newtype Foo' = Foo' { fooEither' :: Either () String }
  deriving Generic

main :: IO ()
main = do
  printJSON (Prelude.Right "bar" :: Prelude.Either () String)
  printJSON (Right "bar" :: Either () String)
  printJSON (Foo (Prelude.Right "baz"))
  printJSON (Foo' (Right "baz"))
  -- Weird, this prints:
  -- {"tag":"Right","value":"bar"}
  -- {"tag":"Right","value":"bar"}
  -- {"fooEither":{"Right":"baz"},"tag":"Foo"}
  -- {"fooEither'":{"contents":"baz","tag":"Right"},"tag":"Foo'"}

  where
    printJSON
      :: ( Generic a, GToJSON' Value Zero (Rep a))
      => a
      -> IO ()
    printJSON
      = do
      let
        sumEncoding 
          = Aeson.defaultTaggedObject
            { Aeson.contentsFieldName = "value"
            }
        options
          = Aeson.defaultOptions
            { Aeson.sumEncoding = sumEncoding
            , Aeson.tagSingleConstructors = True
            , Aeson.unwrapUnaryRecords = True
            }
      ByteString.Char8.putStrLn 
        . ByteString.toStrict
        . Aeson.encode 
        . Aeson.Types.genericToJSON options
