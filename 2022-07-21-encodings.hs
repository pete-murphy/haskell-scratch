{-# LANGUAGE RankNTypes, LambdaCase #-}

import Prelude hiding (Either(..))

data Either a b
  = Left a
  | Right b

type EitherC a b
   = forall c
   . (a -> c)
  -> (b -> c)
  -> c

fromEither :: Either a b -> EitherC a b
fromEither = \case
  Left a -> \left _ -> left a
  Right b -> \_ right -> right b

toEither :: EitherC a b -> Either a b
toEither match = match Left Right


main = undefined
