-- https://github.com/purescript-contrib/purescript-argonaut-codecs/issues/115

{-# LANGUAGE DerivingVia, DeriveAnyClass, DeriveGeneric, UndecidableInstances #-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString as ByteString

import Data.Aeson (GToJSON, GToJSON', Encoding, Value, One, Zero, ToJSON)
-- import GHC.Generics (Generic, Generically, Rep, (:+:))
import GHC.Generics (Generic, Rep, (:+:))
import Generic.Data (Generically)

data MyEither e a = MyLeft e | MyRight a
  deriving Generic
  deriving ToJSON

newtype Foo = Foo { fooEither :: Either () String }
  deriving Generic

newtype Foo' = Foo' { fooEither' :: MyEither () String }
  deriving Generic

main :: IO ()
main = do
  printJSON (Right "bar" :: Either () String)
  printJSON (Foo (Right "baz"))
  printJSON (Foo' (MyRight "baz"))
  -- Weird, this prints:
  -- {"tag":"Right","value":"bar"}
  -- {"fooEither":{"Right":"baz"},"tag":"Foo"}
  -- {"fooEither'":{"contents":"baz","tag":"MyRight"},"tag":"Foo'"}


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
