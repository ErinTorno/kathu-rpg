{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we create some orphan instances for Apecs.Physics types so we can parse them from json files

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Entity.Physics.BodyConfig (BodyConfig, setBodyConfig) where

import Apecs
import Apecs.Physics
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Maybe (maybe)
import Data.Text (Text)

import Kathu.Entity.Physics.CollisionGroup
import Kathu.Entity.Physics.Floor (assignMeWorldFloor, WorldFloor)
import Kathu.Parsing.Aeson ()
    
data BodyConfig = BodyConfig
    { body             :: Body
    , shapes           :: [Convex]
    , sensor           :: Sensor
    , collisionFilter  :: CollisionFilter
    , hasFloorFriction :: Bool
    , density          :: Maybe Density
    , mass             :: Maybe Mass
    , friction         :: Maybe Friction
    , elasticity       :: Maybe Elasticity
    }

setBodyConfig :: forall w m. (MonadIO m, Get w m EntityCounter, Has w m Physics, Has w m WorldFloor, Set w m WorldFloor)
              => Entity -> Maybe BodyConfig -> SystemT w m ()
setBodyConfig ety (Nothing)   = ety $= StaticBody -- if no config given, we at least need to give it a StaticBody so it can have a position
setBodyConfig ety (Just conf) = do
    ety $= (body conf, sensor conf)

    when False $ (set ety . collisionFilter) conf

    let setShape []       = pure ()
        setShape (x:xs)   = ety $= (Shape ety x) >> setShapes xs -- if there is one shape, we can set it on our entity; otherwise, we can give the first shape to the main ety itself
        setShapes mShapes = forM_ mShapes (newEntity . Shape ety) -- otherwise, we create new entities for holding that shape information, linked back to ety
    setShape . shapes $ conf

    when (body conf == DynamicBody) $ do
        -- if we want floor friction, we assign it a world floor that requests for the world on its next update to give us an appropriate floor type
        when (hasFloorFriction conf) $ ety $= assignMeWorldFloor
        maybe (pure ()) (set ety) . density $ conf
        maybe (pure ()) (set ety) . mass $ conf
        maybe (pure ()) (set ety) . friction $ conf
        maybe (pure ()) (set ety) . elasticity $ conf
        set ety          $ Moment (1 / 0)

instance FromJSON Convex where
    parseJSON (Object v) = v .: "shape" >>= mkShape
        where mkShape :: Text -> Parser Convex
              mkShape "custom"    = Convex <$> v .: "vertices" <*> v .: "radius"
              mkShape "circle"    = v .: "radius" >>= \r -> (maybe (oCircle (V2 (-r / 2) (-r))) oCircle <$> v .:? "origin") <*> pure r
              mkShape "rectangle" = v .: "bounds"
                                >>= \bounds@(V2 w h) -> (maybe (oRectangle (V2 (-w / 2) (-h))) oRectangle <$> v .:? "origin") <*> pure bounds
              mkShape e           = fail $ "Convex failed to parse unknown shape of " ++ show e
    parseJSON e          = typeMismatch "Convex" e

instance FromJSON Body where
    parseJSON (String "dynamic")   = pure DynamicBody
    parseJSON (String "kinematic") = pure KinematicBody
    parseJSON (String "static")    = pure StaticBody
    parseJSON e                    = typeMismatch "Body" e

instance FromJSON Density    where parseJSON = (Density<$>) . parseJSON
instance FromJSON Mass       where parseJSON = (Mass<$>) . parseJSON
instance FromJSON Friction   where parseJSON = (Friction<$>) . parseJSON
instance FromJSON Elasticity where parseJSON = (Elasticity<$>) . parseJSON

instance FromJSON BodyConfig where
    parseJSON (Object v) = check =<< BodyConfig
            <$> v .: "type"
            <*> v .: "shapes"
            <*> (mkGroupSensor <$> v .: "group")
            <*> (groupCollisionFilter <$> v .: "group")
            <*> v .:? "has-floor-friction" .!= True
            <*> v .:? "density"
            <*> v .:? "mass"
            <*> v .:? "friction"
            <*> v .:? "elasticity"
        where check :: BodyConfig -> Parser BodyConfig
              check config = (\len -> if len > 0 then pure config else fail ("Attempted to load BodyConfig without any shapes")) . length . shapes $ config
    parseJSON e          = typeMismatch "BodyConfig" e