module Kathu.Util.Flow where

import           Control.Monad.Fail  (MonadFail)
import           Control.Monad.State
import qualified Control.Monad.Fail  as Fail
import           Text.Read           (readMaybe)

-- Functor and similar

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (v1, v2) = (f v1, f v2)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fn <$$> f = fmap fn <$> f

-- Monad

infixl 1 >>>=
(>>>=) :: (Monad m, Monad n) => m (n a) -> (a -> n b) -> m (n b)
(>>>=) v f = v >>= \v' -> pure (v' >>= f)

infixr 1 =<<<
(=<<<) :: (Monad m, Monad n) => (a -> n b) -> m (n a) -> m (n b)
(=<<<) f v = v >>= \v' -> pure (v' >>= f)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []      = pure ([], [])
partitionM fn (x:xs) = fn x >>= app
    where next       = partitionM fn xs
          app True   = (\(u, v) -> (x:u, v)) <$> next
          app False  = (\(u, v) -> (u, x:v)) <$> next

putReturn :: (s, m) -> State s m
putReturn (s, m) = put s >> pure m

readElseFail :: (MonadFail m, Read a) => String -> String -> m a
readElseFail failMsg = ensure . readMaybe
    where ensure Nothing  = Fail.fail failMsg
          ensure (Just x) = pure x