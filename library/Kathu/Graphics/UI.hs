{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Kathu.Graphics.UI where

import           Data.Aeson
import           Data.Aeson.Types        (Parser, typeMismatch)
import           Data.Functor.Compose
import           Data.Text               (Text)
import           Linear.V2               (V2)

import qualified Kathu.Entity.Item       as Item
import           Kathu.Graphics.Drawable (Render, RenderSprite)
import           Kathu.Graphics.Color
import           Kathu.Parsing.Aeson
import           Kathu.Util.Dependency
import           Kathu.Util.Types        (Identifier(..))

-- These two need to have some space for the default attributes and resources, as well as user defined

data UIAttributeType = UIArmor | UIAura | UIMiscAttribute Identifier

data UIResourceType = UIHealth | UIMana | UIMiscResource Identifier

data DisplayBar g = DisplayBar
    -- for every N points that this bar measures, there will be a new unit of the image
    { pointsPerUnit   :: Double
    -- if present this will be drawn at the start and end of the bar
    , barCapBeginning :: Maybe (RenderSprite g)
    , barCapEnding    :: Maybe (RenderSprite g)
    , capWidth        :: Double
    , primaryWidth    :: Double
    , secondaryWidth  :: Double
    , primaryFull     :: RenderSprite g
    , primary3Q       :: RenderSprite g
    , primaryHalf     :: RenderSprite g
    , primary1Q       :: RenderSprite g
    , secondaryFull   :: RenderSprite g
    , secondaryEmpty  :: RenderSprite g
    }

data UIElementConfig g
    = UITextLabel   { uiText           :: Text
                    , uiFontID         :: Identifier
                    , uiFontColor      :: Color }
    | UIAttribute   { uiAttrTitle      :: Text
                    , uiAttributeType  :: UIAttributeType
                    , uiAttrColor      :: Color
                    , uiAttrBonusColor :: Color }
    | UIResource    { uiResTitle       :: Text
                    , uiResourceType   :: UIResourceType
                    , uiResColor       :: Color
                    , uiResMaxColor    :: Color
                    , uiResBonusColor  :: Color }
    | UIResourceBar { uiResBarType     :: UIResourceType
                    , uiDisplayBar     :: DisplayBar g }
    | UIImage       { uiRenderImage    :: Render g }
    | UIItemBox     { uiItemBackground :: g
                    , uiItemContainer  :: Item.ContainerSlot g }

data UIElement g = UIElement
    { uiPosition :: V2 Double
    , uiConfig   :: UIElementConfig g
    }

---------------
-- Instances --
---------------

instance ( FromJSON (Dependency s m (UIElementConfig g))
         , Monad m
         ) => FromJSON (Dependency s m (UIElement g)) where
    parseJSON obj@(Object v) = getCompose $ UIElement <$> v .:^ "position" <*> Compose (parseJSON obj)
    parseJSON e              = typeMismatch "UIElement" e

instance ( FromJSON (Dependency s m g)
         , FromJSON (Dependency s m (Render g))
         , FromJSON (Dependency s m (RenderSprite g))
         , Monad m
         ) => FromJSON (Dependency s m (UIElementConfig g)) where
    parseJSON obj@(Object v) = (v .: "type" :: Parser Text) >>= \case
        "label"             -> getCompose $ UITextLabel   <$> v .:^ "text"  <*> v .:^ "font" <*> v .:^ "color"
        "attribute-label"   -> getCompose $ UIAttribute   <$> v .:^ "title" <*> v .:^ "attribute" <*> v .:^ "color" <*> v .:^ "bonus-color"
        "resource-label"    -> getCompose $ UIResource    <$> v .:^ "title" <*> v .:^ "resource" <*> v .:^ "color" <*> v .:^ "max-color" <*> v .:^ "bonus-color"
        "resource-bar"      -> getCompose $ UIResourceBar <$> v .:^ "resource" <*> Compose (parseJSON obj)
        "image"             -> getCompose $ UIImage       <$> v .:~ "render"
        "item-box"          -> getCompose $ UIItemBox     <$> v .:~ "background" <*> ((flip Item.ContainerSlot) Nothing <$> v .:^ "slot")
        e                   -> fail $ "Unknown UI element type " ++ show e
    parseJSON e              = typeMismatch "UIElementConfig" e
    
instance ToJSON UIResourceType where
    toJSON UIHealth           = toJSON ("health" :: Text)
    toJSON UIMana             = toJSON ("mana" :: Text)
    toJSON (UIMiscResource a) = toJSON a

instance FromJSON UIResourceType where
    parseJSON (String "health") = pure UIHealth
    parseJSON (String "mana")   = pure UIMana
    parseJSON (String e)        = pure . UIMiscResource . Identifier $ e
    parseJSON e                 = typeMismatch "UIResourceType" e

instance ToJSON UIAttributeType where
    toJSON UIArmor             = toJSON ("armor" :: Text)
    toJSON UIAura              = toJSON ("aura" :: Text)
    toJSON (UIMiscAttribute a) = toJSON a

instance FromJSON UIAttributeType where
    parseJSON (String "armor") = pure UIArmor
    parseJSON (String "aura")  = pure UIAura
    parseJSON (String e)       = pure . UIMiscAttribute . Identifier $ e
    parseJSON e                = typeMismatch "UIAttributeType" e

instance (FromJSON (Dependency s m (RenderSprite g)), Monad m) => FromJSON (Dependency s m (DisplayBar g)) where
    parseJSON (Object v) = getCompose $ DisplayBar
        <$> v .:^ "points-per-part"
        <*> v .:~? "cap-beginning"
        <*> v .:~? "cap-ending"
        <*> (v .:^? "cap-width" .!=~ 0) <*> v .:^ "primary-width" <*> v .:^ "secondary-width"
        <*> v .:~ "primary-4q" <*> v .:~ "primary-3q" <*> v .:~ "primary-2q" <*> v .:~ "primary-1q"
        <*> v .:~ "secondary-full" <*> v .:~ "secondary-empty"
    parseJSON v = typeMismatch "DisplayBar" v