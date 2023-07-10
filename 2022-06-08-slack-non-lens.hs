{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Lens (_Object)
import Control.Lens

v :: Value
v = undefined

x = v & _Object . at "x" ?~ (undefined :: Value)
