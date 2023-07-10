{-# LANGUAGE DataKinds, FlexibleInstances, KindSignatures, MultiParamTypeClasses, RankNTypes #-}

import GHC.TypeLits
import Refined
import Data.Coerce (coerce)

data Branded (sym :: Symbol)

instance KnownSymbol sym => Predicate (Branded sym) x where
  validate _ _ = Nothing

type USD = Refined (Branded "USD") Int
type EUR = Refined (Branded "EUR") Int


-- declare const usd: USD
-- declare const num: number
usd :: USD
usd = undefined
num :: Int
num = undefined

-- declare const add2: (n: number) => number
-- declare const convertToEuro: (usd: USD) => EUR
class IsInt n where
  toInt :: n -> Int
instance IsInt Int where
  toInt = id
instance IsInt (Refined x Int) where
  toInt = unrefine
add2 :: IsInt n => n -> Int
add2 = undefined

convertToEuro :: USD -> EUR
convertToEuro = undefined

main = do
  let
    a = add2 num
    b = add2 usd
    -- c = convertToEuro num
    d = convertToEuro usd
  pure ()
