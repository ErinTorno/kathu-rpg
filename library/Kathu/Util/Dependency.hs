{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- Default signature won't compile unless we disable this

{-# LANGUAGE UndecidableInstances #-}

module Kathu.Util.Dependency where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Foldable       as F
import           Data.Kind           (Constraint)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vec

-- | A wrapper around a monad with a state that can be used to retrieve dependencies or store values
newtype Dependency s m a = Dependency (StateT s m a)
    deriving (Functor, Applicative, Monad, MonadState s)

-- | A lighter alternative to CanStore which only states that the dependency must be able to be given, with no expectation to store into it
class CanProvide s a where
    provide :: Monad m => Dependency s m a
    default provide :: (CanStore s a, Monad m) => Dependency s m a
    provide = readStore

type family CanProvideEach s as :: Constraint where
    s `CanProvideEach` '[]       = ()
    s `CanProvideEach` (a ': as) = (CanProvide s a, CanProvideEach s as)

{- | A class used for constaints
An instance can use (instance s `CanStore` Text => MyTypeClass (Dependency s m MyType))
to ensure that we can pull that data from the state information
It is recommended to make use of newtypes for more clear dependencies
-}
class CanProvide s a => CanStore s a where
    storeLens :: Lens' s a
    
    readStore :: Monad m => Dependency s m a
    readStore = gets (view storeLens)

    writeStore :: Monad m => a -> Dependency s m ()
    writeStore val = modify (set storeLens val)

-- Allows us to use (s `CanStoreEach` '[MyTypeA, MyTypeB, MyTypeC]) to ensure multiple types can be stored
type family CanStoreEach s as :: Constraint where
    s `CanStoreEach` '[]       = ()
    s `CanStoreEach` (a ': as) = (CanStore s a, CanStoreEach s as)

---------------
-- Functions --
---------------

runDependency :: Dependency s m a -> s -> m (a, s)
runDependency (Dependency stateT) = runStateT stateT

evalDependency :: Monad m => Dependency s m a -> s -> m a
evalDependency (Dependency stateT) = evalStateT stateT

execDependency :: Monad m => Dependency s m a -> s -> m s
execDependency (Dependency stateT) = execStateT stateT

flattenDependency :: (Monad m, Traversable t) => t (Dependency s m a) -> Dependency s m (t a)
flattenDependency = traverse id

liftDependency :: (Monad m) => m a -> Dependency s m a
liftDependency = Dependency . lift

-- Map related functions; nice to have since we commonly work with Maps when storing data

dependencyMapLookupMap :: (s `CanProvide` Map k a, Monad m, Ord k, Traversable t) => t k -> Dependency s m (Map k a)
dependencyMapLookupMap keys = do
    dmap <- provide
    let adder acc key = addMaybe $ Map.lookup key dmap
            where addMaybe (Just e) = Map.insert key e acc
                  addMaybe Nothing  = acc
    pure $ F.foldl' adder Map.empty keys

dependencyMapLookupVec :: (s `CanProvide` Map k a, Monad m, Ord k, Traversable t) => t k -> Dependency s m (Vector a)
dependencyMapLookupVec keys = do
    dmap <- provide
    let adder acc key = addMaybe acc $ Map.lookup key dmap
        addMaybe acc (Just e) = Vec.cons e acc
        addMaybe acc Nothing  = acc
    pure $ F.foldl' adder Vec.empty keys

dependencyMapLookup :: (s `CanProvide` Map k a, Monad m, Ord k) => k -> Dependency s m (Maybe a)
dependencyMapLookup key = Map.lookup key <$> provide

dependencyMapLookupElseError :: (s `CanProvide` Map k a, Monad m, Ord k, Show k) => String -> k -> Dependency s m a
dependencyMapLookupElseError category key = fromMaybe failMsg <$> dependencyMapLookup key
    where failMsg = error . concat $ [ "Couldn't find element with key "
                                     , show key
                                     , " within "
                                     , category
                                     , " map in the stored dependencies"
                                     ]

dependencyMapInsert :: (s `CanStore` Map k a, Monad m, Ord k) => k -> a -> Dependency s m ()
dependencyMapInsert key value = (Map.insert key value <$> readStore)
                            >>= writeStore

-- a bit specialized, but a very common scenario
storeWithKeyFn :: (s `CanStore` Map k a, Monad m, Ord k) => (a -> k) -> a -> Dependency s m a
storeWithKeyFn getID value = dependencyMapInsert (getID value) value
                          >> pure value