{-# LANGUAGE ExplicitForAll, FlexibleContexts #-}

module Kathu.Physics.System where

import Apecs
import Kathu.Physics.Body

runPhysics :: forall w m. (Get w m Body, Set w m Body) => SystemT w m ()
runPhysics = do
    pure ()