{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Comonad.Store (Store)
import Control.Comonad.Traced (Traced)
import Data.Map.Strict (Map)
-- import Data.Map.Monoidal (MonoidalMap)
import Data.Set (Set)
import Control.Comonad (Comonad)

import qualified Control.Comonad as Comonad
import qualified Control.Comonad.Store as Store
import qualified Control.Comonad.Traced as Traced
import qualified Data.List as List
import qualified Data.Map.Strict as Map
-- import qualified Data.Map.Monoidal as Map
import qualified Data.Set as Set

import Control.Arrow (second)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import qualified Debug.Trace as Debug

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
    go deps result = 
      Debug.traceShow deps
        (deps <> Store.peek deps result)

allDeps' :: Store (Set String) (Set String)
allDeps' = Comonad.kfix (go <$> recipes)
  where
    go :: Set String -> Store (Set String) (Set String) -> (Set String)
    go deps _
      | Set.null deps = mempty
    go deps result = deps <> Store.peek deps result

-----------------

average :: Double -> Double -> Double
average x y = (x + y) / 2

shift :: forall k. Ord k => Map k Double -> Map k Double
shift m = do
  let
    -- Easier to work with a list here, and 
    -- we must sort by _position_ to detect collisions
    list = List.sortOn snd (Map.toList m)
    pairs = zip list (tail list)

    -- This never shows
    _ = Debug.traceShow (map (snd . snd) pairs)
    
  Map.fromList (f =<< pairs)
  where
    -- Compute all the changes to make
    f ((k, x), (k', y)) = do
     let diff = y - x
     if diff < 1 then 
       let z = average x y
        in [(k, z - 0.5), (k', z + 0.5)]
       else [(k, x), (k', y)]

allShifted :: forall k. Ord k => Store (Map k Double) (Map k Double)
allShifted = do
  Comonad.extend Comonad.wfix (go <$> store)
  where
    store = Store.store shift Map.empty
    go :: Map k Double -> Store (Map k Double) (Map k Double) -> Map k Double
    go shifts _
      | Map.null shifts = Map.empty
    go shifts result = do
      shifts <> (Store.peek shifts result)

allShifted' :: forall k. Ord k => Traced (Map k Double) (Map k Double)
allShifted' = do
  Comonad.extend Comonad.wfix (go <$> traced)
  where
    traced :: Traced (Map k Double) (Map k Double)
    traced = Traced.traced shift
    go :: Map k Double -> Traced (Map k Double) (Map k Double) -> Map k Double
    go shifts _
      | Map.null shifts = Map.empty
    go shifts result = do
      -- Traced.runTraced result shifts 
      shifts <> Traced.runTraced result shifts 



main :: IO ()
main = do
  -- Currently results in: 
  -- _|_ non-termination
  -- print (Traced.runTraced  allShifted' (Map.fromList [('a', 1), ('b', 3), ('c', 5)]))

  print (Store.peek (Set.fromList ["quiver"]) allDeps)
