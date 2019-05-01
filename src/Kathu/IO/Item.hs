{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.IO.Item where

import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser, Value)
import Data.Maybe
import Data.Functor.Compose
import Data.Text (Text)
import Kathu.Entity.Item
import Kathu.IO.Graphics
import Kathu.IO.Parsing
import Kathu.Util ((>>>=))

-- need custom implementation, as even with project options the type isn't converted to the correct case
instance ToJSON Slot where
    toJSON UseItem   = String "use-item"
    toJSON Weapon    = String "weapon"
    toJSON Head      = String "head"
    toJSON Torso     = String "torso"
    toJSON Legs      = String "legs"
    toJSON Accessory = String "accessory"
    toJSON NoSlot    = String "no-slot"
instance FromJSON Slot where
    parseJSON (String "use-item")  = pure UseItem
    parseJSON (String "weapon")    = pure Weapon
    parseJSON (String "head")      = pure Head
    parseJSON (String "torso")     = pure Torso
    parseJSON (String "legs")      = pure Legs
    parseJSON (String "accessory") = pure Accessory
    parseJSON (String "no-slot")   = pure NoSlot
    parseJSON v = typeMismatch "Slot" v
    
instance FromJSON (SystemLink Item) where
    parseJSON (Object v) = itemPar >>>= \it -> insertSL items (itemID it) it
        where itemPar :: Parser (SystemLink Item)
              itemPar = getCompose $ Item
                  <$> v .:^ "id"
                  <*> v .:^ "name"
                  <*> v .:^ "description"
                  <*> v .:^ "slot"
                  <*> v .:~ "icon"
                  <*> v .:^ "max-stack-size"
                  <*> v .:^ "price"
    parseJSON v = typeMismatch "Item" v

instance FromJSON (SystemLink ItemStack) where
    parseJSON (Object v) = getCompose $ ItemStack <$> item <*> v .:^ "count"
        where item :: Compose Parser SystemLink Item
              item = Compose $ lookupSingle items <$> v .: "item"
    parseJSON v          = typeMismatch "ItemStack" v

instance FromJSON (SystemLink ContainerSlot) where
    parseJSON (Object v) = getCompose $ ContainerSlot <$> v .:^? "slot" <*> item
        where maybeLookup :: Maybe Text -> SystemLink (Maybe Item)
              maybeLookup = maybe (pure Nothing) (fmap Just . lookupSingle items)
              item :: Compose Parser SystemLink (Maybe Item)
              item = Compose $ maybeLookup <$> v .:? "item"
    parseJSON v          = typeMismatch "ContainerSlot" v

instance FromJSON (SystemLink Container) where
    parseJSON (Object v) = getCompose $ Container <$> Compose equipSlots <*> Compose miscSlots
        where equipSlots = v .: "equip-slots" >>= concatSlots
              miscSlots  = v .: "misc-slots"  >>= concatSlots
              concatSlots :: Value -> Parser (SystemLink [ContainerSlot])
              concatSlots ara@(Array _) = getCompose $ concat <$> (Compose $ parseListSLWith parseSlots ara)
              concatSlots _             = error "Container slots are not in the form of an array" 
              parseSlots :: Value -> Parser (SystemLink [ContainerSlot])
              parseSlots obj@(Object innerV) = (\i csl -> csl >>= pure . replicate i) <$> innerV .: "count" <*> parseJSON obj
              parseSlots _                   = error "Container slot is not in the form of an object"
    parseJSON v          = typeMismatch "Container" v

instance FromJSON (SystemLink DeathDrop) where
    parseJSON (Object v) = getCompose $ DeathDrop <$> v .:^ "chance" <*> v .:^ "count" <*> item
        where item :: Compose Parser SystemLink Item
              item = Compose $ lookupSingle items <$> v .: "item"
    parseJSON v          = typeMismatch "DeathDrop" v

instance FromJSON (SystemLink Inventory) where
    parseJSON obj@(Object v) = getCompose . parseInv =<< v .: "type"
        where parseInv :: Text -> Compose Parser SystemLink Inventory
              parseInv "container"   = Compose $ (>>=(pure . InvContainer)) <$> parseJSON obj
              parseInv "death-drops" = Compose $ (>>=(pure . InvDeathDrops)) <$> parseListSL obj
              parseInv t             = error $ "Attempted to parse Inventory with unknown type: " ++ show t
    parseJSON v          = typeMismatch "Inventory" v