{-# LANGUAGE ApplicativeDo #-}

import Control.Applicative

op :: (Applicative f, Monoid m, Semigroup s) => f (m, s) -> f (m -> (m, s)) -> f (m, s)
op = flip (liftA2 (\f (m, s) -> let (m', s') = f m in (m', s <> s')))

op'' :: (Applicative f, Monoid m) => f (m, a) -> f (m -> (m, a)) -> f (m, a)
op'' = flip (liftA2 (\f (m, _) -> f m))
--
-- op fma fmma = (\f (m, _) -> f m) <$> fmma <*> fma
--
-- op fma fmma = ($) <$> fmma <*> (fst <$> fma)
--
--  do
--  mma <- fmma
--  (m, _) <- fma
--  pure (mma m)

op' :: (Applicative f, Monoid m) => f (m, a) -> f (m -> (m, a)) -> f (m, a)
op' = flip (liftA2 (\f (m, a) -> let (m', _) = f m in (m', a)))
--
-- op' fma fmma = (\f (m, a) -> let (m', _) = f m in (m', a)) <$> fmma <*> fma
--
--do
--  mma <- fmma
--  (m, a) <- fma
--  let (m', _) = mma m
--  pure (m', a)



fun x = op a b
  where
    a = (mempty, ) <$> x
    b = (\(l, r) o -> (mappend o l, r)) <$> fun x

main = undefined
