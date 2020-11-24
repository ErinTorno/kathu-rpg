{-# LANGUAGE UndecidableInstances #-}

module Kathu.Entity.Item where

import Apecs
import Data.Aeson
import Data.Aeson.Types        (typeMismatch, Parser)
import Data.Functor.Compose
import Data.Maybe              (fromMaybe)
import Data.Text               (Text)
import GHC.Generics
import Verda.Graphics.Sprites  (Sprite)
import Verda.Parsing.Aeson
import Verda.Util.Dependency
import Verda.Util.Flow         ((>>>=))
import Verda.Util.Types        (Identifier, IDMap, Range)

data ItemSlot = UseItem | Weapon | Body | SpiritCharm | Accessory | NoSlot deriving (Show, Eq, Ord, Enum, Generic)

data SpecialCategory
    = NonUnique
    | KeyItem          -- Important, do not let the player get rid of this
    | WorldSpaceShared -- Linked to the current worldspace, rather than the player
    deriving (Show, Eq, Generic)

data Item = Item
    { itemID      :: Identifier
    , itemName    :: Text
    , description :: Text
    , itemSlot    :: ItemSlot
    , itemIcon    :: Sprite
    , stackSize   :: Int
    , price       :: Int
    , autoPickup  :: Bool
    , specialCategory :: SpecialCategory
    -- Effects?
    }

data ItemStack = ItemStack {stackItem :: Item, stackCount :: Int}

data ContainerSlot = ContainerSlot {slotRestriction :: Maybe ItemSlot, heldItem :: Maybe Item}

data Container = Container
    { invEquippedItems :: [ContainerSlot]
    , invMiscItems     :: [ContainerSlot]
    }

data DeathDrop = DeathDrop
    { dropChance :: Double
    , dropCount  :: Range Int
    , dropItem   :: Item
    }

data Inventory = InvContainer Container | InvDeathDrops [DeathDrop]

instance Component Inventory where type Storage Inventory = Map Inventory

-------------------
-- Serialization --
-------------------

-- Serialization for this module is located here
-- This is due to its length and complexity compared to other modules

-- need custom implementation, as even with project options the type isn't converted to the correct case
instance ToJSON ItemSlot where
    toJSON slot = case slot of
        UseItem     -> String "use-item"
        Weapon      -> String "weapon"
        Body        -> String "body"
        SpiritCharm -> String "spirit-charm"
        Accessory   -> String "accessory"
        NoSlot      -> String "no-slot"
instance FromJSON ItemSlot where
    parseJSON = withText "Slot" $ \case
        "use-item"     -> pure UseItem
        "weapon"       -> pure Weapon
        "body"         -> pure Body
        "spirit-charm" -> pure SpiritCharm
        "accessory"    -> pure Accessory
        "no-slot"      -> pure NoSlot
        "misc"         -> pure NoSlot
        e              -> fail $ "Attempted to invalid item slot " ++ show e
    
instance ToJSON SpecialCategory where
    toJSON category = case category of
        WorldSpaceShared -> String "worldspace-shared"
        KeyItem          -> String "key-item"
        NonUnique        -> String "non-unique"
instance FromJSON SpecialCategory where
    parseJSON = withText "SpecialCategory" $ \case
        "worldspace-shared" -> pure WorldSpaceShared
        "key-item"          -> pure KeyItem
        "non-unique"        -> pure NonUnique
        e                   -> fail $ "Attempted to parse item with invalid special category " ++ show e

-- Item

instance ( s `CanStore` IDMap Item
         , FromJSON (Dependency s m Sprite)
         , Monad m
         ) => FromJSON (Dependency s m Item) where
    parseJSON (Object v) = itemPar >>>= storeWithKeyFn itemID
        where itemPar = getCompose $ Item
                  <$> v .:^ "item-id"
                  <*> v .:^ "name"
                  <*> v .:^ "description"
                  <*> v .:^ "slot"
                  <*> v .:- "icon"
                  <*> v .:^ "max-stack-size"
                  <*> v .:^ "price"
                  <*> (v .:^? "auto-pickup" .!=- False)
                  <*> v .:^? "special-category" .!=- NonUnique
    parseJSON v = typeMismatch "Item" v

-- ItemStack

instance (s `CanProvide` IDMap Item, Monad m) => FromJSON (Dependency s m ItemStack) where
    parseJSON (Object v) = getCompose $ ItemStack <$> item <*> v .:^? "count" .!=- 1
        where item    = Compose (fmap (fromMaybe failMsg) . dependencyMapLookup <$> (v .: "item" :: Parser Identifier))
              failMsg = error "Couldn't find item referenced by ItemStack"
    parseJSON v          = typeMismatch "ItemStack" v

-- ContainerSlot

instance (s `CanProvide` IDMap Item, Monad m) => FromJSON (Dependency s m ContainerSlot) where
    parseJSON (Object v) = getCompose $ ContainerSlot <$> v .:^? "slot" <*> item
        where maybeLookup = maybe (pure Nothing) (fmap (maybe failMsg Just) . dependencyMapLookup)
              item = Compose $ maybeLookup <$> (v .:? "item" :: Parser (Maybe Identifier))
              failMsg = error "Couldn't find item referenced by ContainerSlot"
    parseJSON v          = typeMismatch "ContainerSlot" v

-- Container

instance (FromJSON (Dependency s m ContainerSlot), Monad m) => FromJSON (Dependency s m Container) where
    parseJSON (Object v) = getCompose $ Container <$> Compose equipSlots <*> Compose miscSlots
        where equipSlots = v .: "equip-slots" >>= concatSlots
              miscSlots  = v .: "misc-slots"  >>= concatSlots
              concatSlots ara@(Array _) = getCompose $ concat <$> (Compose $ parseListDPWith parseSlots ara)
              concatSlots _             = error "Container slots are not in the form of an array" 
              parseSlots obj@(Object innerV) = fmap . replicate <$> innerV .: "count" <*> parseJSON obj
              parseSlots _                   = error "Container slot is not in the form of an object"
    parseJSON v          = typeMismatch "Container" v

-- DeathDrop

instance (s `CanProvide` IDMap Item, Monad m) => FromJSON (Dependency s m DeathDrop) where
    parseJSON (Object v) = getCompose $ DeathDrop <$> v .:^ "chance" <*> v .:^ "count" <*> item
        where item    = Compose (fmap (fromMaybe failMsg) . dependencyMapLookup <$> (v .: "item" :: Parser Identifier))
              failMsg = error "Couldn't find item referenced by DeathDrop"
    parseJSON v          = typeMismatch "DeathDrop" v

-- Inventory

instance ( FromJSON (Dependency s m Container)
         , FromJSON (Dependency s m DeathDrop)
         , Monad m
         ) => FromJSON (Dependency s m Inventory) where
    parseJSON obj@(Object v) = getCompose . parseInv =<< (v .: "type" :: Parser Text)
        where parseInv "container"   = Compose $ (>>=(pure . InvContainer)) <$> parseJSON obj
              parseInv "death-drops" = Compose $ (>>=(pure . InvDeathDrops)) <$> parseJSON obj
              parseInv t             = error $ "Attempted to parse Inventory with unknown type: " ++ show t
    parseJSON v          = typeMismatch "Inventory" v