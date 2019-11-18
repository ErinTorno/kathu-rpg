{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Kathu.Entity.Physics.Floor where

import           Apecs                  hiding (get, Map)
import           Apecs.Physics          hiding (Map)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.Functor.Compose
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Word
import           Linear.V2              (V2(..))

import           Kathu.Parsing.Aeson
import           Kathu.Parsing.Counting
import           Kathu.Util.Apecs
import           Kathu.Util.Dependency
import           Kathu.Util.Types       (Identifier)

-------------
-- FloorID --
-------------

newtype FloorID = FloorID {unFloorID :: Word32} deriving (Show, Eq, Ord)

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m FloorID) where
    parseJSON (String s) = pure (FloorID . fromIntegral <$> lookupOrAdd "FloorID" s)
    parseJSON v          = typeMismatch "FloorID" v

-- | If an entity has this floor ID, it lets the game know that the entity associated with it is invalid, and to create a new one as soon as possible
assignMeFloorID :: FloorID
assignMeFloorID = FloorID 0

-- | CountingIDs is initialized with this key and default map, instead of empty, so that it starts counting after reserved tile IDs
reservedFloorIDMap :: (Text, Map Text Int)
reservedFloorIDMap = ("FloorID", Map.fromList [("assign-me", 0)])

----------------------
-- Entity Component --
----------------------

data WorldFloor = WorldFloor
    { wFloorID            :: {-# UNPACK #-} !FloorID
    , wFloorConstraintEty :: {-# UNPACK #-} !Entity
    } deriving (Show, Eq)

assignMeWorldFloor :: WorldFloor
assignMeWorldFloor = WorldFloor assignMeFloorID 0

isFloorUnassigned :: WorldFloor -> Bool
isFloorUnassigned (WorldFloor (FloorID 0) _) = True
isFloorUnassigned _ = False

-------------------
-- FloorProperty --
-------------------

data FloorProperty = FloorProperty
    { propFloorID   :: {-# UNPACK #-} !FloorID
    , propTextID    ::                !Identifier
    , propMaxForce  :: {-# UNPACK #-} !Double
    -- possible sound/some visuals in the future?
    } deriving (Show, Eq)

data FloorPropEntity = FloorPropEntity
    { propEtyEntity :: {-# UNPACK #-} !Entity
    , propEtyFloor  :: {-# UNPACK #-} !FloorProperty
    } deriving (Show, Eq)

instance (FromJSON (Dependency s m FloorID), Monad m) => FromJSON (Dependency s m FloorProperty) where
    parseJSON (Object v) = getCompose $ FloorProperty
        <$> v .:~ "floor-id"
        <*> v .:^ "floor-id"
        <*> v .:^ "max-force"
    parseJSON v          = typeMismatch "FloorProperty" v

initFloorProperty :: forall w m. (MonadIO m, Get w m EntityCounter, Has w m Physics, HasEach w m '[Body, Position])
                  => FloorProperty -> SystemT w m FloorPropEntity
initFloorProperty = ((FloorPropEntity <$> newEntity (StaticBody, Position $ V2 0 0))<*>) . pure

assignWorldFloor :: forall w m. (MonadIO m, Get w m EntityCounter, Has w m Physics, HasEach w m '[WorldFloor])
                 => FloorPropEntity -> (WorldFloor, Entity) -> SystemT w m WorldFloor
assignWorldFloor (FloorPropEntity fety (FloorProperty fid _ force)) ((WorldFloor wid wety), ety) = do
    -- if wid isn't assignMe, then that means it has a valid constraint entity that needs to be removed first
    when (wid /= assignMeFloorID) $
        destroy wety (Proxy :: Proxy Constraint)
    constraintEty <- newEntity (Constraint fety ety $ PivotJoint2 (V2 0 0) (V2 0 0), MaxBias 0, MaxForce force)
    pure (WorldFloor fid constraintEty)