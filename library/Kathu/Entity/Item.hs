{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Entity.Item where

import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Functor.Compose
import Data.Text (Text)
import GHC.Generics

import Kathu.Entity.Components
import Kathu.Parsing.Aeson
import Kathu.Util.Dependency
import Kathu.Util.Flow ((>>>=))
import Kathu.Util.Types (Identifier, IDMap, Range)

data Slot = UseItem | Weapon | Head | Torso | Legs | Accessory | NoSlot deriving (Show, Eq, Generic)

data SpecialCategory = NonUnique | KeyItem | WorldSpaceShared deriving (Show, Eq, Generic)

-- allows use to compare slot types
instance Ord Slot where
    compare a b = slotPriority a `compare` slotPriority b
    (<=) a b    = slotPriority a <= slotPriority b

data Item g = Item
    { itemID      :: Identifier
    , itemName    :: Text
    , description :: Text
    , slot        :: Slot
    , itemIcon    :: Render g
    , stackSize   :: Int
    , price       :: Int
    , specialCategory :: SpecialCategory
    -- Effects?
    }

data ItemStack g = ItemStack {stackItem :: Item g, stackCount :: Int}

data ContainerSlot g = ContainerSlot {restriction :: Maybe Slot, heldItem :: Maybe (Item g)}

data Container g = Container
    { invEquippedItems :: [ContainerSlot g]
    , invMiscItems     :: [ContainerSlot g]
    }

data DeathDrop g = DeathDrop
    { dropChance :: Double
    , dropCount :: Range Int
    , dropItem :: Item g
    }

data Inventory g = InvContainer (Container g) | InvDeathDrops [DeathDrop g]

-----------------------
-- Utility functions --
-----------------------

-- when we sort and inventory or items, use this order
slotPriority :: Slot -> Int
slotPriority UseItem   = 0
slotPriority Weapon    = 1
slotPriority Head      = 2
slotPriority Torso     = 3
slotPriority Legs      = 4
slotPriority Accessory = 5
slotPriority NoSlot    = 6

-------------------
-- Serialization --
-------------------

-- Serialization for this module is located here
-- This is due to its length and complexity compared to other modules

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
    parseJSON (String "misc")      = pure NoSlot
    parseJSON (String s)           = error . concat $ ["Attempted to parse item with invalid slot \"", show s, "\""]
    parseJSON v = typeMismatch "Slot" v
    
instance ToJSON SpecialCategory where
    toJSON WorldSpaceShared = String "worldspace-shared"
    toJSON KeyItem          = String "key-item"
    toJSON NonUnique        = String "non-unique"
instance FromJSON SpecialCategory where
    parseJSON (String "worldspace-shared") = pure WorldSpaceShared
    parseJSON (String "key-item")          = pure KeyItem
    parseJSON (String "non-unique")        = pure NonUnique
    parseJSON (String s)                   = error . concat $ ["Attempted to parse item with invalid special category \"", show s, "\""]
    parseJSON v = typeMismatch "Slot" v

-- Item

instance (s `CanStore` IDMap (Item g), FromJSON (Dependency s m (Render g)), Monad m) => FromJSON (Dependency s m (Item g)) where
    parseJSON (Object v) = itemPar >>>= storeWithKeyFn itemID
        where itemPar = getCompose $ Item
                  <$> v .:^ "item-id"
                  <*> v .:^ "name"
                  <*> v .:^ "description"
                  <*> v .:^ "slot"
                  <*> v .:~ "icon"
                  <*> v .:^ "max-stack-size"
                  <*> v .:^ "price"
                  <*> v .:^? "special-category" .!=~ NonUnique
    parseJSON v = typeMismatch "Item" v

-- ItemStack

instance (s `CanProvide` IDMap (Item g), Monad m) => FromJSON (Dependency s m (ItemStack g)) where
    parseJSON (Object v) = getCompose $ ItemStack <$> item <*> v .:^? "count" .!=~ 1
        where item    = (Compose (fmap (maybe failMsg id) . dependencyMapLookup <$> (v .: "item" :: Parser Identifier))) 
              failMsg = error "Couldn't find item referenced by ItemStack"
    parseJSON v          = typeMismatch "ItemStack" v

-- ContainerSlot

instance (s `CanProvide` IDMap (Item g), Monad m) => FromJSON (Dependency s m (ContainerSlot g)) where
    parseJSON (Object v) = getCompose $ ContainerSlot <$> v .:^? "slot" <*> item
        where maybeLookup = maybe (pure Nothing) (fmap (maybe failMsg Just) . dependencyMapLookup)
              item = Compose $ maybeLookup <$> (v .:? "item" :: Parser (Maybe Identifier))
              failMsg = error "Couldn't find item referenced by ContainerSlot"
    parseJSON v          = typeMismatch "ContainerSlot" v

-- Container

instance (FromJSON (Dependency s m (ContainerSlot g)), Monad m) => FromJSON (Dependency s m (Container g)) where
    parseJSON (Object v) = getCompose $ Container <$> Compose equipSlots <*> Compose miscSlots
        where equipSlots = v .: "equip-slots" >>= concatSlots
              miscSlots  = v .: "misc-slots"  >>= concatSlots
              concatSlots ara@(Array _) = getCompose $ concat <$> (Compose $ parseListDPWith parseSlots ara)
              concatSlots _             = error "Container slots are not in the form of an array" 
              parseSlots obj@(Object innerV) = (\i csl -> csl >>= pure . replicate i) <$> innerV .: "count" <*> parseJSON obj
              parseSlots _                   = error "Container slot is not in the form of an object"
    parseJSON v          = typeMismatch "Container" v

-- DeathDrop

instance (s `CanProvide` IDMap (Item g), Monad m) => FromJSON (Dependency s m (DeathDrop g)) where
    parseJSON (Object v) = getCompose $ DeathDrop <$> v .:^ "chance" <*> v .:^ "count" <*> item
        where item    = (Compose (fmap (maybe failMsg id) . dependencyMapLookup <$> (v .: "item" :: Parser Identifier))) 
              failMsg = error "Couldn't find item referenced by DeathDrop"
    parseJSON v          = typeMismatch "DeathDrop" v

-- Inventory

instance ( FromJSON (Dependency s m (Container g))
         , FromJSON (Dependency s m (DeathDrop g))
         , Monad m
         ) => FromJSON (Dependency s m (Inventory g)) where
    parseJSON obj@(Object v) = getCompose . parseInv =<< (v .: "type" :: Parser Text)
        where parseInv "container"   = Compose $ (>>=(pure . InvContainer)) <$> parseJSON obj
              parseInv "death-drops" = Compose $ (>>=(pure . InvDeathDrops)) <$> parseListDP obj
              parseInv t             = error $ "Attempted to parse Inventory with unknown type: " ++ show t
    parseJSON v          = typeMismatch "Inventory" v