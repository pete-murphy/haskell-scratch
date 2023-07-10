{-# LANGUAGE GADTs, InstanceSigs #-}

import Control.Comonad

data Fold a b
  -- | @Fold @ @ step @ @ initial @ @ extract@
  = forall x. Fold (x -> a -> x) x (x -> b)

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)

instance Comonad (Fold e) where
    extract (Fold _ begin done) = done begin
    duplicate (Fold step begin done) = Fold step begin (\x -> Fold step x done)
    extend :: (Fold e a -> b) -> Fold e a -> Fold e b
    extend f (Fold step begin done)
      = Fold step begin (\x -> f (Fold step x done))

--  extend :: (w a -> b) -> w a -> w b
--  extend f = fmap f . duplicate

main = undefined
