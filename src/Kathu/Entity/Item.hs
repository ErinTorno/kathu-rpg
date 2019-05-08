{-# LANGUAGE DeriveGeneric #-}

module Kathu.Entity.Item where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Kathu.Graphics.Drawable (RenderSprite)
import Kathu.Util.Misc

data Slot = UseItem | Weapon | Head | Torso | Legs | Accessory | NoSlot deriving (Show, Eq, Generic)

-- allows use to compare slot types
instance Ord Slot where
    compare a b = slotPriority a `compare` slotPriority b
    (<=) a b    = slotPriority a <= slotPriority b

data Item = Item
    { itemID      :: Text
    , itemName    :: Text
    , description :: Text
    , slot        :: Slot
    , itemIcon    :: RenderSprite
    , stackSize   :: Int
    , price       :: Int
    -- Effects?
    }

data ItemStack = ItemStack {stackItem :: Item, stackCount :: Int}

data ContainerSlot = ContainerSlot {restriction :: Maybe Slot, heldItem :: Maybe Item}

data Container = Container
    { invEquippedItems :: [ContainerSlot]
    , invMiscItems     :: [ContainerSlot]
    }

data DeathDrop = DeathDrop
    { dropChance :: Double
    , dropCount :: Range Int
    , dropItem :: Item
    }

data Inventory = InvContainer Container | InvDeathDrops [DeathDrop]

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