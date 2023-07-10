{-# LANGUAGE TypeSynonymInstances, UndecidableInstances #-}

newtype Curried   a b c = Curried (a -> b -> c)
  deriving Functor

newtype Uncurried a b c = Uncurried ((a, b) -> c)
  deriving Functor

class Iso f g where
  to :: forall a. f a -> g a
  from :: forall a. g a -> f a

instance Iso (Curried a b) (Uncurried a b) where
  to :: forall c. Curried a b c -> Uncurried a b c
  to (Curried curried) = Uncurried (uncurry curried)
  from (Uncurried uncurried) = Curried (curry uncurried)

class (Functor f, Functor g) => Adjunction f g where
  left :: forall a b. (f a -> b) -> a -> g b
  right :: forall a b. (a -> g b) -> f a -> b

newtype State s a = State (s -> (a, s))
  deriving Functor
  
newtype Store s a = Store (s -> a, s)
  deriving Functor

-- instance Adjunction 

-- instance (Iso (f a -> b) (a -> g b), Functor f, Functor g) => Adjunction f g

-- instance (Iso f g, Functor f, Functor g) => Adjunction f g where
--   left fab = \a -> 

main = undefined
