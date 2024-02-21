{-# LANGUAGE BlockArguments #-}


class Applicative f => Selective f where
    select :: f (Either a b) -> f (a -> b) -> f b

-- | Keep running an effectful computation until it returns a @Right@ value,
-- collecting the @Left@'s using a supplied @Monoid@ instance.
untilRight_reference :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
untilRight_reference x = select y h
  where
    -- y :: f (Either a (a, b))
    y = fmap (mempty,) <$> x
    -- h :: f (a -> (a, b))
    h = (\(as, b) a -> (mappend a as, b)) <$> untilRight_reference x

untilRight :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
untilRight x = do
  let 
    y = fmap (mempty,) <$> x
    r = untilRight x
    h = (\(as, b) a -> (mappend a as, b)) <$> r
  select y h

untilRight' :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
untilRight' = (\x -> go x id) 
  where
  go x cont = do
    let y = fmap (mempty,) <$> x
    go x \x' -> do
      let h = (\(as, b) a -> (mappend a as, b)) <$> x'
      cont (select y h)

untilRight'' :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
untilRight'' = (\x -> go x id) 
  where
  go x cont = do
    let y = fmap (mempty,) <$> x
    go x (cont' y cont)

  cont' y cont = \x' -> do
    let h = (\(as, b) a -> (mappend a as, b)) <$> x'
    cont (select y h)

--------------

data Accum f a b
  = Cont' (f (Either a (a, b))) (Accum f a b)
  | Id 


untilRight''' :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
untilRight''' = (\x -> go x Id) 
  where
  go x cont = do
    let y = fmap (mempty,) <$> x
    go x (Cont' y cont)

    -- Accum f0 a0 b0 -> f0 (a0, b0) -> f0 (a0, b0)

  eval cont x = case cont of
    Cont' y next -> do
      let h = (\(as, b) a -> (mappend a as, b)) <$> x
      eval next (select y h) 
      -- ??? eval cont (select y h)
      -- Nate says using `next` here is a typo, but I'm not sure it is
    Id ->
      x



-----------------



---------------- DUMB

_untilRight''' :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
_untilRight''' = (\x -> go x Id) 
  where
  go x cont = do
    let y = fmap (mempty,) <$> x
    go x (Cont' y cont)

  eval cont x = case cont of
    Cont' y next -> do
      let h = (\(as, b) a -> (mappend a as, b)) <$> x
      eval next (select y h) 
      -- ??? eval cont (select y h)
      -- Nate says using `next` here is a typo, but I'm not sure it is
    Id ->
      x

data Call f a b
  = Go (f (Either a b)) (Accum f a b)
  | Eval (f (Either a b)) (Accum f a b)

{-

_untilRight'''' :: (Monoid a, Selective f) => f (Either a b) -> f (a, b)
_untilRight'''' = (\x -> go (Go x Id))
  where
  go call = case call of
    Go x cont -> do
      let y = fmap (mempty,) <$> x
      go (Go x (Cont' y cont))
    Eval x cont -> case cont of
      Cont' y next -> do
        let h = (\(as, b) a -> (mappend a as, b)) <$> x
        go (Eval next (select y h))
      Id ->
        x
-}

main = undefined
