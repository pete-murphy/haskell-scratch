{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

import Data.Function
import Control.Arrow
import Data.List
import Data.Foldable
import Data.Maybe
import Control.Monad

data MaxBy a b = MaxBy a b
  deriving (Show, Functor, Foldable)

instance (Ord a, Semigroup b) => Semigroup (MaxBy a b) where
  x@(MaxBy a b) <> y@(MaxBy a' b')
    | a == a'   = MaxBy a (b <> b')
    | a > a'    = x
    | otherwise = y

maxesBy :: Ord b => (a -> b) -> [a] -> [a]
maxesBy f = 
  (\(MaxBy _ a) -> a)
    <=< maybeToList
      . foldMap (Just . uncurry MaxBy . (f &&& singleton))

main = do
  let 
    x = maxesBy length ["abc", "def", "gh"]
  print x
  -- ["abc","def"]

  let 
    y = maxesBy length ["abc", "def", "ghij"]
  print y
  -- ["ghij"]


--    &
--  let
--    xs = MaxBy 'b' [1]
--      <> MaxBy 'b' [2]
--      <> MaxBy 'a' [1]
--  print xs
--  let
--    ys = MaxBy 'b' [1]
--      <> MaxBy 'b' [2]
--      <> MaxBy 'c' [1]
--  print ys
