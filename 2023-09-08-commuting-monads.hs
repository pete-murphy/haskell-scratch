{-# LANGUAGE BlockArguments #-}

import Control.Monad.Reader
import Control.Applicative

import Control.Monad.Identity

newtype WeirdStateT s m a = WeirdStateT (s -> (m a, s))

type WeirdStateReader s r = WeirdStateT s (Reader r)

instance Functor m => Functor (WeirdStateT s m) where
  fmap f (WeirdStateT sma) = WeirdStateT \s -> do
    let (ma, s') = sma s
    (f <$> ma, s')
  
instance Applicative m => Applicative (WeirdStateT s m) where
  pure x = WeirdStateT \s -> (pure x, s)
  liftA2 f (WeirdStateT sma) (WeirdStateT smb) = WeirdStateT \s -> do
    let
      (ma, s') = sma s
      (mb, s'') = smb s'
    (f <$> ma <*> mb, s'')

instance Monad m => Monad (WeirdStateT s m) where
  WeirdStateT sma >>= f = WeirdStateT \s -> do
    let
      (ma, s') = sma s
    (,s') do
      a <- ma
      let
        WeirdStateT g = f a
        (mb, _) = g s'
      mb

    

--  fmap f (WeirdStateT sma) = WeirdStateT \s -> do
--    let (ma, s') = sma s
--    (f <$> ma, s')

main = do
  do 
    let 
      h :: String -> WeirdStateT Int Identity String
      h str = WeirdStateT \n -> (pure (str <> show n), n + 1)

      x = pure "foo" >>= h
      x' = h "foo"
    print (case x of WeirdStateT z -> z 0)
    print (case x' of WeirdStateT z -> z 0)
  do
    let
      m :: WeirdStateT Int Identity String
      m = WeirdStateT \n -> (pure "poo", n + 2)

      x = m >>= pure
    print (case m of WeirdStateT z -> z 0)
    print (case x of WeirdStateT z -> z 0)
  pure ()


