{-# LANGUAGE RecordWildCards  #-}

module Z (X(..), fn, main) where

data X
  = X
    { foo :: String
    , bar :: Int
    }

fn :: X -> String
fn X {..} = foo

main :: IO ()
main = pure ()
