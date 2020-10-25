module Verda.Util.Flow where

import           Control.Applicative (liftA2)

-- Functor and similar

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fn <<$>> f = fmap fn <$> f

infixl 4 <<*>>
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

-- Monad

infixl 1 >>>=
(>>>=) :: (Monad m, Monad n) => m (n a) -> (a -> n b) -> m (n b)
(>>>=) v f = v >>= \v' -> pure (v' >>= f)

infixr 1 =<<<
(=<<<) :: (Monad m, Monad n) => (a -> n b) -> m (n a) -> m (n b)
(=<<<) f v = v >>= \v' -> pure (v' >>= f)

infixl 1 >>=/
(>>=/) :: (Monad m) => m a -> (a -> m b) -> m a
(>>=/) v f = v >>= \val -> f val >> pure val 

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []      = pure ([], [])
partitionM fn (x:xs) = fn x >>= app
    where next       = partitionM fn xs
          app True   = (\(u, v) -> (x:u, v)) <$> next
          app False  = (\(u, v) -> (u, x:v)) <$> next

ireplicateM_ :: Monad m => Int -> (Int -> m a) -> m ()
ireplicateM_ nTimes action = go nTimes
    where go 0  = pure ()
          go !i = action i >> go (i - 1)