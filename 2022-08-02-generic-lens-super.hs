{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, TypeApplications, OverloadedLabels #-}

import Data.Generics.Product
import GHC.Generics
import Control.Lens

data BazSub
  = BazSub
    { baz1 :: Int
    , baz2 :: Int
    } deriving Generic

data BazSup
  = BazSup
    { baz1 :: Int
    } deriving Generic

data ThingSub
  = ThingSub
    { foo :: String
    , bar :: Int
    , baz :: BazSub
    } deriving Generic

data ThingSup
  = ThingSup
    { foo :: String
    , baz :: BazSup
    } deriving Generic

thingSub = ThingSub "foo" 1 bazSub
bazSub = BazSub 99 100

thingSup = ThingSup "bar" bazSup
bazSup = BazSup 0

-- x = smash thingSup thingSub
-- x = smash (thingSup { baz = smash (thingSup ^. #baz) (thingSub ^. #baz) }) thingSub
baz' = smash bazSup bazSub 
x = smash (thingSup { baz = baz' }) thingSub

main = undefined
