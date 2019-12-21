{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Kathu.Util.Apecs where

import           Apecs
import           Apecs.Core            (explGet, explMembers, explSet)
import           Control.Monad         (when)
import           Data.Kind             (Constraint)
import qualified Data.Vector.Unboxed   as U

import           Kathu.Util.Collection (fromJustElseError)

type family ReadWrite w m c :: Constraint where
    ReadWrite w m c = (Get w m c, Has w m c, Set w m c)

type family ReadWriteEach w m cs :: Constraint where
    ReadWriteEach w m '[]       = ()
    ReadWriteEach w m (c ': cs) = (Has w m c, Set w m c, Get w m c, ReadWriteEach w m cs)

type family HasEach w m cs :: Constraint where
    HasEach w m '[]       = ()
    HasEach w m (c ': cs) = (Has w m c, HasEach w m cs)

type family GetEach w m cs :: Constraint where
    GetEach w m '[]       = ()
    GetEach w m (c ': cs) = (Get w m c, GetEach w m cs)

type family SetEach w m cs :: Constraint where
    SetEach w m '[]       = ()
    SetEach w m (c ': cs) = (Set w m c, SetEach w m cs)

getUnique :: forall w m c. (Members w m c, Get w m c) => SystemT w m (Maybe c)
getUnique = cfold (\_ c -> Just c) Nothing

getUniqueElseError :: forall w m c. (Members w m c, Get w m c) => String -> SystemT w m c
getUniqueElseError errMsg = fromJustElseError errMsg <$> getUnique

-- Same logic as Apec's cmapIf implementation, just with f returning in a monad
{-# INLINE cmapIfM #-}
cmapIfM :: forall w m cp cx cy. (Get w m cx, Get w m cp, Members w m cx, Set w m cy)
        => (cp -> Bool) -> (cx -> SystemT w m cy) -> SystemT w m ()
cmapIfM cond f = do
    pStore :: Storage cp <- getStore
    xStore :: Storage cx <- getStore
    yStore :: Storage cy <- getStore
    cs     <- lift . explMembers $ (xStore, pStore)
    U.forM_ cs $ \ety -> do
        p <- lift . explGet pStore $ ety
        when (cond p) $ do
            x <- lift . explGet xStore $ ety
            y <- f x
            lift . explSet yStore ety $ y