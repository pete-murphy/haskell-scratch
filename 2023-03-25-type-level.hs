{-# LANGUAGE DataKinds, KindSignatures #-}

-- data N = N0 | N1
--   deriving (Eq)
-- 
-- data A (t :: N) = A { a :: N }
-- 
-- zero :: A 'N0
-- zero = A N0
-- 
-- one :: A 'N1
-- one = A N1
-- 
-- isZero :: N -> Bool
-- isZero = (==) N0
-- 
-- func :: N -> 
-- 
--
import Data.Either (Either)
import Data.Generic.Rep (Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

data Bar a b m c
  = Bar1 (Maybe a)
  | Bar2 (Either a b)
  | Bar3 a
  | Bar4 { myMonadicResult :: m b }

main = pure ()
