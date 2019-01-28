module Entity.Item where

import Data.Text (Text)
import qualified Data.Text as T

data Slot = UseItem | Head | Body | Accessory | NoSlot deriving (Show, Eq)

-- when we sort and inventory or items, use this order
slotPriority :: Slot -> Int
slotPriority UseItem   = 0
slotPriority Head      = 1
slotPriority Body      = 2
slotPriority Accessory = 3
slotPriority NoSlot    = 4

-- allows use to compare slot types
instance Ord Slot where
    compare a b = slotPriority a `compare` slotPriority b
    (<=) a b    = slotPriority a <= slotPriority b

data Item = Item
    { itemID      :: Text
    , description :: Text
    , slot        :: Slot
    , stackSize   :: Int
    , price       :: Int
    -- Effects?
    }

data ItemStack = ItemStack {stackItem :: Item, stackCount :: Int}

data InventorySlot = InventorySlot {restriction :: Maybe Slot, heldItem :: Maybe Item}