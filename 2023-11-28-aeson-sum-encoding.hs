{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (contentsFieldName, defaultOptions, encode, sumEncoding, SumEncoding(..), ToJSON)
import Data.Aeson.Types (genericToJSON)
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Functor.Identity (Identity(..))
import GHC.Generics (Generic)
import Prelude hiding (putStrLn)

data MySum a b = A a | B b
  deriving (Generic, ToJSON)

main :: IO ()
main = do
  let
    options = defaultOptions
        { sumEncoding = TaggedObject "boop" "blah" }
    x = Identity (B "baz" :: MySum () String)
  putStrLn (encode (genericToJSON options x))


