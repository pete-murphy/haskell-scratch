{-# LANGUAGE TypeApplications, BlockArguments #-}

import Data.Foldable
import Data.List.Split
import Control.Arrow
import Data.Function
import Data.Functor
import Data.List
import Data.Char

avg ns = fromIntegral (sum ns) / fromIntegral (length ns)

distFrom :: Double -> (String, Int) -> Double
distFrom x (_, n) = abs (fromIntegral n - x)

-- $> main

main = do
  file <- readFile "source.txt"
  let input = file
        & lines
        & chunksOf 4
        & map (head &&& last)
        & map (second (filter isDigit))
        & map (second (read @Int))
      ns = map snd input
      avg' = avg ns
      sorted = (sortOn @Double) (distFrom avg') input
      rollingAvg = reverse (avg <$> inits ns)
  putStrLn ("Avg: " <> show avg')
  for_ (take 10 (zip [1..] sorted)) \(i, (name, amt)) -> do
    putStrLn (show i <> ". " <> name <> " - " <> show amt)
  for_ (take 20 (map (show . round) rollingAvg)) putStrLn
  putStrLn "\n"
