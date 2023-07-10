{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Comonad.Store (Store)
import Data.Map (Map)
-- import Data.Map.Monoidal (MonoidalMap)
import Data.Set (Set)

import qualified Control.Comonad as Comonad
import qualified Control.Comonad.Store as Store
import qualified Data.List as List
import qualified Data.Map as Map
-- import qualified Data.Map.Monoidal as Map
import qualified Data.Set as Set

import Control.Arrow (second)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

ingredientsOf :: String -> Set String
ingredientsOf = \case
  "string"  -> Set.fromList ["wool"]
  "sticks"  -> Set.fromList ["wood"]
  "bow"     -> Set.fromList ["sticks", "string"]
  "arrows"  -> Set.fromList ["sticks", "feathers", "stone"]
  "quiver"  -> Set.fromList ["arrows", "bow"]
  "torches" -> Set.fromList ["coal", "sticks"]
  _         -> mempty

recipes :: Store (Set String) (Set String)
recipes = Store.store (foldMap ingredientsOf) mempty

allDeps :: Store (Set String) (Set String)
allDeps = Comonad.extend Comonad.wfix (go <$> recipes)
  where
    go :: Set String -> Store (Set String) (Set String) -> (Set String)
    go deps _
      | Set.null deps = mempty
    go deps result = deps <> Store.peek deps result

-----------------

average :: Double -> Double -> Double
average x y = (x + y) / 2
--  let
--    -- Easier to work with a list here, and 
--    -- we must sort by _position_ to detect collisions
shift :: forall k. Ord k => ((k, Double), (k, Double)) -> Map k Double
shift ((k, x), (k', y)) = do
  let
    xs = do
      let diff = y - x
      if diff < 1 then 
        let z = average x y
         in [(k, z - 0.6), (k', z + 0.6)]
        else [(k, x)]
  Map.fromList xs

shiftStore :: forall k. Ord k => Store (Map k Double) (Map k Double)
shiftStore = Store.store shift' Map.empty
  where
    shift' m = do
      let
        list = List.sortOn snd (Map.toList m)
        pairs = zip list (tail list)
      Map.unionsWith average (map shift pairs)
   

allShifted :: forall k. Ord k => Store (Map k Double) (Map k Double)
allShifted = Comonad.extend Comonad.wfix (go <$> shiftStore)
  where
    go :: Map k Double -> Store (Map k Double) (Map k Double) -> Map k Double
    go shifts _
      | Map.null shifts = Map.empty
    go shifts result = shifts <> (Store.peek shifts result)


main :: IO ()
main = do
  -- print (Store.peek (Set.fromList ["quiver"]) allDeps)
  print (Store.peek (Map.fromList [('a', 1), ('b', 2)]) allShifted)
