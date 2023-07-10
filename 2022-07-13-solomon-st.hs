{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}

import Data.Kind (Type)
import Control.Monad.Indexed
import Prelude hiding ((>>=), pure, read)

newtype ST r i o a = ST { unST :: (Int, i) -> (Int, o, a) }
  deriving Functor

newtype Ref a = Ref (Int, a)

instance IxFunctor (ST r) where
  imap = fmap

instance IxPointed (ST r) where
  ireturn x = ST \(n, r) -> (n, r, x)

pure :: forall r i x. x -> ST r i i x
pure = ireturn

instance IxApplicative (ST r) where
  iap (ST f) (ST g) = ST \nr -> do
    let (n, r, fab) = f nr
        (n', r', a) = g (n, r)
    (n', r', fab a)

instance IxMonad (ST r) where
  ibind f (ST ma) = ST \nr -> do
    let (n, r, a) = ma nr
        ST g = f a
    g (n, r)

(ST ma) >>= f = ibind f (ST ma)

class STCons (x :: Type) (i :: Type) (o :: Type) | x i -> o where
  stCons :: Int -> x -> i -> o

class TypeEq (a :: Type) (b :: Type) (e :: Bool)  | a b -> e

instance                                     TypeEq a a True
instance {-# OVERLAPPABLE #-} (e ~ False) => TypeEq a b e


instance STCons a ([(Int, a)], x) ([(Int, a)], x) where
  stCons n x (l, r) = (((n, x) : l), r)
instance STCons b x o => STCons b ([(Int, a)], x) ([(Int, a)], o) where
  stCons n x (l, r) = (l, stCons n x r)
instance (a ~ b) => STCons a [(Int, b)] [(Int, b)] where
  stCons n x l = (n, x) : l
instance {-# OVERLAPPABLE #-} STCons a [(Int, b)] ([(Int, a)], [(Int, b)]) where
  stCons n x r = ([(n, x)], r)

class STModify (x :: Type) (y :: Type) where
  stModify :: (x -> x) -> Int -> x -> y -> (x, y)

instance STModify a ([(Int, a)], x) where
  stModify f n x (l, y) = let (a, b) = stModify f n x l in (a, (b, y))

instance STModify b x => STModify b ([(Int, a)], x) where
  stModify f n x (y, l) = let (a, b) = stModify f n x l in (a, (y, b))

instance STModify a [(Int, a)] where
  stModify f ix v l' = go l'
    where
    go ((n, x) : l) = comp n ix n x l
    go [] = (v, [])
    comp n 0 0 x l = let fx = f x in (fx, (n, fx) : l)
    comp n j k x l
      | j > 0 && k > 0 = comp n (j-1) (k-1) x l
      | otherwise      = let (a, b) = go l in (a, (n, x) : b)
    -- comp n j k x l = comp n (j-1) (k-1) x l
    -- comp n _ 0 x l = let (a, b) = go l in (a, (n, x) : b)
    -- comp n 0 _ x l = let (a, b) = go l in (a, (n, x) : b)

new :: forall a r i o. STCons a i o => a -> ST r i o (Ref a)
new a = ST \(n, r) -> do
  let ix = n + 1
  let newR = stCons ix a r
  (ix, newR, (Ref (ix, a)))

read :: forall r a i. STModify a i => Ref a -> ST r i i a
read (Ref (ix, v)) = ST \(n, r) ->
  (n, r, (let (a, _) = stModify (\z -> z) ix v r in a))

write :: forall r a i. STModify a i => a -> Ref a -> ST r i i ()
write v (Ref (ix, o)) = ST \(n, r) ->
  (n, (let (_, b) = stModify (\_ -> v) ix o r in b), ())

modify :: forall r a i. STModify a i => (a -> a) -> Ref a -> ST r i i a
modify f (Ref (ix, v)) = ST \(n, r) -> do
  let (a, b) = stModify f ix v r
  (n, b, a)

modify_ :: forall r a i. STModify a i => (a -> a) -> Ref a -> ST r i i ()
modify_ f r = do
  _ <- modify f r
  pure ()

type STU :: forall k. k -> Type -> Type -> Type
type STU r o a = ST r [(Int, ())] o a

runST :: forall a o. (forall r. STU r o a) -> a
runST (ST f) = let (_, _, c) = f (0, [(0, ())]) in c

-- test

data Vehicle = Boat | Car | Train
data Composer = Bach | Beethoven | Brahms
data Color = Red | Green | Blue

colorVehicle :: forall r. STU r _ (Color, Vehicle)
colorVehicle = do
  _ <- new Green
  myColor <- new Blue
  write Red myColor
  color2 <- read myColor
  myVehicle <- new Car
  write Green myColor
  _ <- new Green
  write color2 myColor
  outColor <- read myColor
  outVehicle <- read myVehicle
  _ <- new Beethoven
  write Boat myVehicle
  pure (outColor, outVehicle)

myColorVehicle :: (Color, Vehicle)
myColorVehicle = runST colorVehicle

main = do
   undefined
