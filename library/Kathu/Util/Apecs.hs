{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Kathu.Util.Apecs where

import Apecs
import Data.Kind (Constraint)

type family HasEach w m cs :: Constraint where
    HasEach w m '[]       = ()
    HasEach w m (c ': cs) = (Has w m c, Set w m c, Get w m c, HasEach w m cs)

type family GetEach w m cs :: Constraint where
    GetEach w m '[]       = ()
    GetEach w m (c ': cs) = (Get w m c, GetEach w m cs)

type family SetEach w m cs :: Constraint where
    SetEach w m '[]       = ()
    SetEach w m (c ': cs) = (Set w m c, SetEach w m cs)