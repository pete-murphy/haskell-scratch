{-# LANGUAGE NumericUnderscores, BlockArguments #-}

import Control.Concurrent
import Data.Monoid

f_ :: Int -> IO Bool
f_ n = do
  threadDelay 100_000
  pure (n == 5)

x_traverse :: [Int] -> IO Bool
x_traverse = fmap or . traverse f_

x_foldMap :: [Int] -> IO Bool
x_foldMap = fmap getAny . getAp <$> foldMap \n -> Ap $ Any <$> f_ n

