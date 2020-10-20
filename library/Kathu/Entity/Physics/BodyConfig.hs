{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- we create some orphan instances for Apecs.Physics types so we can parse them from json files

module Kathu.Entity.Physics.BodyConfig (BodyConfig, setBodyConfig) where

import Apecs
import Apecs.Physics
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as DSet
import Data.Text (Text)

import Kathu.Entity.Components (Existance, Tags(..), newExistingEntity)
import Kathu.Entity.LifeTime
import Kathu.Entity.Physics.CollisionGroup
import Kathu.Entity.Physics.Floor (assignMeWorldFloor, WorldFloor)
import Verda.Parsing.Aeson ()
import           Verda.Util.Apecs

data BodyShape = BodyShape
    { shapeConvex :: !Convex
    , shapeFilter :: !CollisionFilter
    , shapeSensor :: !Sensor
    , shapeTag    :: !(Maybe Text)
    }

data BodyConfig = BodyConfig
    { body             :: Body
    , shapes           :: [BodyShape]
    , hasFloorFriction :: Bool
    , density          :: Maybe Density
    , mass             :: Maybe Mass
    , friction         :: Maybe Friction
    , elasticity       :: Maybe Elasticity
    }

setBodyConfig :: forall w m. (MonadIO m, Get w m EntityCounter, Has w m Physics, ReadWriteEach w m [Existance, LifeTime, WorldFloor, Tags])
              => Entity -> Maybe BodyConfig -> SystemT w m ()
setBodyConfig ety Nothing     = ety $= StaticBody -- if no config given, we at least need to give it a StaticBody so it can have a position
setBodyConfig ety (Just conf) = do
    ety $= body conf

    let mkShape :: Maybe LifeTime -> BodyShape -> (Shape, CollisionFilter, Sensor, Maybe LifeTime, Maybe Tags)
        mkShape lf (BodyShape conv fil sens tag) = (Shape ety conv, fil, sens, lf, Tags . DSet.singleton <$> tag) -- if the parent has a lifetime, we should inherit that too

        setShapes _ []      = pure ()
        setShapes lf (x:xs) = do
            let tag = shapeTag x
            maybeTags <- get ety

            -- if both the shapes and the entity have tags, we need to append the tag instead
            -- possible issue, if we check for a tag the first collision shapes tag would be included, and vice versa
            -- unlikely to ever matter, and they are usually defined in example same place
            when (isJust (shapeTag x) && isJust maybeTags) $ do 
                let Tags tags = fromJust maybeTags
                ety $= Tags (DSet.insert (fromJust tag) tags)
                
            -- if there is one shape, we can set it on our entity; otherwise, we can give the first shape to the main ety itself
            ety $= mkShape lf x
            -- for the rest, we create new entities for holding that shape information, linked back to ety
            forM_ xs $ newExistingEntity . mkShape lf
    
    hasLifeTime <- exists ety (Proxy :: Proxy LifeTime)
    if hasLifeTime then do
        lifetime <- get ety
        setShapes (Just lifetime) . shapes $ conf
    else
        setShapes Nothing . shapes $ conf

    when (body conf == DynamicBody) $ do
        -- if we want floor friction, we assign it a world floor that requests for the world on its next update to give us an appropriate floor type
        when (hasFloorFriction conf) $ ety $= assignMeWorldFloor
        maybe (pure ()) (set ety) . density $ conf
        maybe (pure ()) (set ety) . mass $ conf
        maybe (pure ()) (set ety) . friction $ conf
        maybe (pure ()) (set ety) . elasticity $ conf
        set ety          $ Moment (1 / 0)

instance FromJSON BodyShape where
    parseJSON (Object v) = do
        let mkConvex :: Text -> Parser Convex
            mkConvex "custom"    = Convex <$> v .: "vertices" <*> v .: "radius"
            mkConvex "circle"    = v .: "radius" >>= \r -> (maybe (oCircle (V2 (-r / 2) (-r))) oCircle <$> v .:? "origin") <*> pure r
            mkConvex "rectangle" = v .: "bounds"
                               >>= \bounds@(V2 w h) -> (maybe (oRectangle (V2 (-w / 2) (-h))) oRectangle <$> v .:? "origin") <*> pure bounds
            mkConvex e           = fail $ "Convex failed to parse unknown shape of " ++ show e
        category   <- v .: "category"
        convexType <- v .: "shape"
        tag        <- v .:? "tag"
        convex     <- mkConvex convexType
        pure $ BodyShape convex (groupCollisionFilter category) (mkGroupSensor category) tag
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
            <*> v .:? "has-floor-friction" .!= True
            <*> v .:? "density"
            <*> v .:? "mass"
            <*> v .:? "friction"
            <*> v .:? "elasticity"
        where check :: BodyConfig -> Parser BodyConfig
              check config = (\len -> if len > 0 then pure config else fail "Attempted to load BodyConfig without any shapes") . length . shapes $ config
    parseJSON e          = typeMismatch "BodyConfig" e