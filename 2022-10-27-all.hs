module Main where

import Control.Arrow


all' = runKleisli (Kleisli (const val) >>> arr (arr foo &&& arr bar >>> uncurry (&&))) ()
  where
  val :: IO a
  val = undefined
  foo :: a -> Bool
  foo = undefined
  bar :: a -> Bool
  bar = undefined

main = pure ()
