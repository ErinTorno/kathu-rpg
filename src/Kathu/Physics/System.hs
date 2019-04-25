{-# LANGUAGE ExplicitForAll, FlexibleContexts #-}

module Kathu.Physics.System where

import Apecs
import Kathu.Physics.Body

runPhysics :: forall w m. (Get w m (Body Entity), Set w m (Body Entity)) => SystemT w m ()
runPhysics = do
    return ()