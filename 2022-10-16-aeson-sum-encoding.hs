{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy.Char8 as BL

newtype ModuleName = ModuleName Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Qualified a = Qualified (Maybe ModuleName) a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Qualified)

main :: IO ()
main = do
  BL.putStrLn $ encode (Qualified Nothing ("foo" :: Text))
  BL.putStrLn $ encode (Qualified (Just $ ModuleName "A") ("bar"::Text))
