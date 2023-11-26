module Main where

import qualified Data.Map as Map
import Data.Bifoldable (bifoldMap)

main :: IO ()
main = do
  let
    m = Map.fromList
         [(1,"b"), (2,"c"), (0,"a")]
  print (bifoldMap (\k -> show k <> ":") (\v -> v <> ",") m)

