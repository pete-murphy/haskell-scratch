{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

main = do
  T.putStrLn $ T.decodeUtf8 $ T.encodeUtf8 $ T.pack "👇"
  T.putStrLn $ T.decodeUtf8 $ T.encodeUtf8 $ T.pack "👇🏿"

  -- T.print $ T.decodeLatin1 $ T.encodeUtf8 $ T.pack "👇🏿"
  -- T.putStrLn $ T.decodeLatin1 $ T.encode $ T.pack "👇🏿"


