{-# LANGUAGE BlockArguments, MultiWayIf, OverloadedLists, OverloadedStrings, ViewPatterns #-}

import qualified Data.Csv as Csv
import Control.Arrow
import Data.Csv (Header)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Foldable (for_)
import Data.Map (Map, (!?), (!))
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List
import Data.Maybe
import Control.Lens
import Data.Ord

-- process :: [[(Text, Text)]] -> IO ()
-- process xs = do
-- --  for_ xs \((_,x):rest) -> do
-- --    for_ rest \(y, amount) ->
-- --      Text.IO.putStrLn (y <> " owes " <> x <> " " <> amount)
--   let ps = xs >>= \((_,x):rest) -> rest <&> \(y, amount) -> (y, x, read (Text.unpack amount) :: Double)
--   for_ (filter (\(_,_,n) -> n > 0) ps) print
-- 
--   pure ()

main = do
--  contents <- ByteString.Lazy.readFile "/Users/Pete/Downloads/Copy of Bachelor Party - Sheet6.csv"
--  let xs :: Either String (Header, (Vector (Map Text Text)))
--      xs = Csv.decodeByName contents
--  case xs of
--    Left err -> print err
--    Right (_, input) -> do
--      let res = Vector.toList input <&> Map.assocs
--          res' :: [(String, String, Double)] 
--          res' = res >>= \((_,x):rest) -> rest <&> \(y, amount) -> (Text.unpack y, Text.unpack x, read (Text.unpack amount) :: Double)
--          res'' :: Map String (Map String Double)
--          res'' = (res' <&> \(x, y, n) -> (x, Map.fromList [(y, n)]))
--                   & Map.fromListWith (<>)
--      pure ()
  pure ()

iprint :: (FoldableWithIndex i t, Show i, Show a) => t a -> IO ()
iprint = itraverse_ \i a -> putStrLn (show i <> ": " <> show a)

--example :: Map String (Map String Double)
--example =
--  [ ("A", [("B", 1)])
--  , ("B", [("C", 2)])
--  , ("C", [("A", 5)])
--  ]
example :: [(String, String, Double)]
example =
  [ ("A", "B", 1)
  , ("B", "C", 2)
  , ("C", "A", 5)
  ]

--  Compute the net amount for every person. The net amount for person ‘i’ can be computed by subtracting sum of all debts from sum of all credits.
--  Find the two persons that are maximum creditor and maximum debtor. Let the maximum amount to be credited maximum creditor be maxCredit and maximum amount to be debited from maximum debtor be maxDebit. Let the maximum debtor be Pd and maximum creditor be Pc.
--  Find the minimum of maxDebit and maxCredit. Let minimum of two be x. Debit ‘x’ from Pd and credit this amount to Pc
--  If x is equal to maxCredit, then remove Pc from set of persons and recur for remaining (n-1) persons.
--  If x is equal to maxDebit, then remove Pd from set of persons and recur for remaining (n-1) persons.

-- sortBy (comparing snd) $ map (id &&& (flip netAmt example)) ["A", "B", "C"]


process :: (Ord k, Ord v, Num v) => [(k, k, v)] -> [(k, k, v)]
process xs =
  let 
    ks = keys xs
    sorted = sortBy (comparing snd) $ map (id &&& (flip netAmt xs)) ks
    
   in xs

netAmt
  :: (Ord k, Ord v, Num v) 
  => k
  -> [(k, k, v)]
  -> v
netAmt k = 
  sum . mapMaybe \(i, j, n) ->
    if | k == i -> Just (-n)
       | k == j -> Just n
       | otherwise -> Nothing

keys :: [(k, k, v)] -> [k]
keys xs = xs >>= \(i,j,_) -> [i,j]

-- reduce
--   :: (Ord k, Ord v, Num v) 
--   => k                   -- "A"
--   -> Map k v             -- [("B", 1)]
--   -> Map k (Map k v)     -- [("B", [("C", 2)])
--                          -- ,("C", [("A", 5)])
--                          -- ]
--   -> Map k (Map k v)
-- reduce k xs xss =
--   let ys = xss
--    in ys 


-- $> addLookup [("a", 1), ("b", 2)] "a" "b"
-- $> addLookup' [("a", 1), ("b", 2)] "a" "b"

addLookup env var1 var2
   | Just val1 <- lookup var1 env
   , Just val2 <- lookup var2 env
   = val1 + val2

addLookup' env var1 var2 = case (env, env) of
  (lookup var1 -> Just val1, lookup var2 -> Just val2) -> val1 + val2
