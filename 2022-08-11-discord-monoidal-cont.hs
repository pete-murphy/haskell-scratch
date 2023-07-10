import Control.Monad.Trans.Cont

withAllFolded' :: Monoid a => [(a -> b) -> b] -> (a -> b) -> b
withAllFolded' withFuncs = runCont . fmap mconcat $ traverse cont withFuncs
