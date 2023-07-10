{-# LANGUAGE LambdaCase, PatternSynonyms #-}

import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON(..), FromJSON(..))
import Control.Applicative ((<|>))

data Pair = Pair Int Int

pattern NullPair :: Pair
pattern NullPair = Pair 0 0

instance ToJSON Pair where
  toJSON = \case 
    NullPair -> Aeson.Null
    Pair a b -> toJSON (a, b)

instance FromJSON Pair where
  parseJSON = \case
    Aeson.Null -> pure NullPair
    value -> do
      (x, y) <- parseJSON value
      pure (Pair x y)

main :: IO ()
main = do
  pure ()
