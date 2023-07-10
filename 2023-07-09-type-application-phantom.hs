{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Kind
import Data.Proxy

class Three x y z | x y -> z, x z -> y, y z -> x 

fn1 :: forall a b c. Three a b c => a -> ()
fn1 _ = ()

data Dict
  (c :: Type -> Type -> Type -> Constraint)
  (x :: Type)
  (y :: Type)
  (z :: Type)
  where
    Dict :: c x y z => Dict c x y z

fn2 :: forall a b c. Three a b c => a -> ()
fn2 a = fn1 @a @b a

-- fn3 :: forall a b b' c c'. Three a b c => Three a b' c' => a -> ()
-- fn3 a = fn1 a

main :: IO ()
main = pure ()
