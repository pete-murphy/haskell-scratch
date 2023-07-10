instance Num b => Num (a -> b) where
  f + g = \x -> f x + g x
  f * g = \x -> f x * g x
  abs f = \x -> abs (f x)
  signum f = \x -> signum (f x)
  fromInteger = \i -> \_ -> fromInteger i
  negate f = \x -> negate (f x)

main = do
  print $ (2 + 2) True
  
