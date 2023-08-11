module Main where

mapMaybeWriteOld :: Traversable f => (a -> Maybe a) -> f a -> ([a], f a)
mapMaybeWriteOld f l =
  -- traverse 
  --   :: (a -> ([a], a))
  --   -> f a
  --   -> ([a], f a)
  (traverse :: ()) (\a -> maybe ([], a) (\a' -> ([a], a')) (f a)) l


x :: () 
x = maybe

main :: IO ()
main = pure ()

