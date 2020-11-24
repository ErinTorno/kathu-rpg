{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.WorldSpace where

import           Apecs                     hiding (Map)
import qualified Apecs
import           Apecs.Physics             hiding (Map)
import           Control.Lens              hiding ((.=))
import           Control.Monad             (foldM)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types          (Parser, typeMismatch)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import           Verda.IO.Directory        (WorkingDirectory)
import           Verda.Parsing.Yaml        (FieldOrder, mkFieldOrderFromList)
import           Verda.System.Tile.Chunks  (unitsPerTile)
import           Verda.System.Tile.Components
import           Verda.Util.Dependency
import           Verda.Util.Types          (Identifier, IDMap)

import           Kathu.Entity.Item
import           Kathu.Entity.Prefab       (Prefab, prefabID)
import           Kathu.Graphics.Palette
import qualified Kathu.Scripting.Lua.Types as Lua
import           Kathu.Scripting.Variables
import           Kathu.World.ChunkBuilder
import           Kathu.World.Tile          (AllTiles', Tile)

data InstancedPrefab = InstancedPrefab
    { _instanceID         :: !Identifier
    , _basePrefab         :: Prefab
    , _spawnLocation      :: !(V2 Double)
    , _wireSignalEmitter  :: !(Maybe Identifier)
    , _wireSignalReceiver :: !(Maybe Identifier)
    , _instanceConfig     :: !(IDMap WorldVariable)
    }
makeLenses ''InstancedPrefab

-- | Marks an entity as having been created by an instance prototype specified by the loaded Worldspace; the Int is for the prototype's index
newtype EditorInstancedFromWorld = EditorInstancedFromWorld {unEditorInstancedFromWorld :: InstancedPrefab}
-- Should be just a map, and is expected to be entirely empty when running the game normally
instance Component EditorInstancedFromWorld where type Storage EditorInstancedFromWorld = Apecs.Map EditorInstancedFromWorld

emptyInstancedPrefab :: InstancedPrefab
emptyInstancedPrefab = InstancedPrefab "" prototype (V2 0 0) Nothing Nothing Map.empty
    where prototype = error "Attempted to use emptyInstancedPrefab basePrefab; no possible value exists for this"

data InstancedItem = InstancedItem
    { _baseItem     :: ItemStack
    , _itemPosition :: !(V2 Double)
    }
makeLenses ''InstancedItem

data WorldSpace = WorldSpace
    { _worldID            :: !Identifier
    , _worldName          :: !Text
    , _initialPalette     :: !Identifier
    , _worldPalettes      :: !(IDMap Palette)
    , _loadPoint          :: !(V2 Double)
    , _shouldSavePosition :: !Bool -- when serialized, should we remember where the player was?
    , _worldVariables     :: !(IDMap WorldVariable)
    , _worldScript        :: !(Maybe Lua.Script)
    , _worldEntities      :: !(Vector InstancedPrefab)
    , _worldItems         :: !(Vector InstancedItem)
    , _worldChunks        :: !Chunks
    , _worldInventory     :: !Identifier
    }
makeLenses ''WorldSpace

instance Semigroup WorldSpace  where (<>) = mappend
instance Monoid WorldSpace  where mempty = emptyWorldSpace
instance Component WorldSpace  where type Storage WorldSpace  = Global WorldSpace

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" "the void..." "" Map.empty (V2 0 0) False Map.empty Nothing Vec.empty Vec.empty emptyChunks "void"

-- | A collections of obtained items that are specific to certain worlds
newtype WorldInventory = WorldInventory {unWorldInventory :: IDMap (IDMap ItemStack)}

instance Semigroup WorldInventory  where (<>) = mappend
instance Monoid WorldInventory  where mempty = WorldInventory Map.empty
instance Component WorldInventory  where type Storage WorldInventory  = Global WorldInventory

-------------------
-- Serialization --
-------------------

instance ToJSON InstancedPrefab where
    toJSON (InstancedPrefab idt ety pos sigEmit sigRece config) = object
        [ "entity"           .= prefabID ety
        , "instance-id"      .= (if idt == "" then Nothing else Just idt)
        , "position"         .= (pos / unitsPerTile)
        , "emitting-signal"  .= sigEmit
        , "receiving-signal" .= sigRece
        , "config"           .= (if Map.null config then Nothing else Just config)
        ]

instance (s `CanProvide` (IDMap Prefab), Monad m) => FromJSON (Dependency s m InstancedPrefab) where
    parseJSON (Object obj) = do
        idt     <- obj .:? "instance-id" .!= ""
        pos     <- (*unitsPerTile) <$> obj .: "position"
        sigEmit <- obj .:? "emitting-signal"
        sigRece <- obj .:? "receiving-signal"
        config  <- obj .:? "config" .!= Map.empty
        entity  <- dependencyMapLookupElseError "Entity" <$> (obj .: "entity" :: Parser Identifier)
        pure $ (\e -> InstancedPrefab idt e pos sigEmit sigRece config) <$> entity
    parseJSON e = typeMismatch "InstancedPrefab" e

instance ToJSON InstancedItem where
    toJSON (InstancedItem (ItemStack item count) pos) = object
        [ "item"     .= itemID item
        , "count"    .= (if count == 1 then Nothing else Just count)
        , "position" .= (pos / unitsPerTile)
        ]

instance (FromJSON (Dependency s m ItemStack), Functor m) => FromJSON (Dependency s m InstancedItem) where
    parseJSON val@(Object obj) = do
        pos     <- (*unitsPerTile) <$> obj .: "position"
        stack   <- parseJSON val
        pure $ flip InstancedItem pos <$> stack
    parseJSON e = typeMismatch "InstancedItem" e

----------------
-- WorldSpace --
----------------

encodeValueForWorldSpace :: MonadIO m => AllTiles' -> WorldSpace -> m Value
encodeValueForWorldSpace allTiles (WorldSpace wid wname initPal palettes loadPos shouldSavePos vars script etys items chunks inventory) = do
    fieldPairs <- serializeFieldSetPairs allTiles chunks
    pure . object $
        [ "world-id"                   .= wid
        , "name"                       .= wname
        , "script"                     .= script
        , "initial-palette"            .= initPal
        , "palettes"                   .= Map.keys palettes
        , "load-point"                 .= (loadPos / unitsPerTile)
        , "should-save-exact-position" .= shouldSavePos
        , "inventory-id"               .= inventory
        , "variables"                  .= vars
        , "entities"                   .= Vec.reverse etys  -- as part of loading this get reversed, so we un-reverse it once we save to ensure the same order
        , "items"                      .= Vec.reverse items
        ]
        ++ fieldPairs

instance ( s `CanProvide` IDMap Prefab
         , s `CanProvide` IDMap Palette
         , s `CanProvide` IDMap Tile
         , s `CanProvide` WorkingDirectory
         , FromJSON (Dependency s m ItemStack)
         , MonadIO m
         ) => FromJSON (Dependency s m WorldSpace) where
    parseJSON (Object v) = do
        wID        <- v .: "world-id"
        wName      <- v .: "name"
        initPal    <- v .: "initial-palette"
        paletteIDs <- v .: "palettes"
        loadPnt    <- (*unitsPerTile) <$> v .: "load-point"
        variables  <- v .:? "variables" .!= Map.empty
        worldInvId <- fromMaybe wID <$> v .:? "inventory-id"

        shouldSerializePosition <- v .:? "should-save-exact-position" .!= False
        chunkConfigs :: Vector ChunkConfig <- v .: "chunks"
        
        wScript :: Dependency s m (Maybe Lua.Script) <- flattenDependency <$> v .:? "script"

        dLegend :: Dependency s m MapLegend <- do
            (keys, vals) :: ([Text], [Identifier]) <- unzip . Map.toList <$> v .: "legend"

            pure . fmap (Map.fromList . zip (T.head <$> keys))
                 . flattenDependency
                 . fmap (dependencyMapLookupElseError "Tile")
                 $ vals

        items    <- v .: "items"
        entities <- v .: "entities"

        let palettes = dependencyMapLookupMap (paletteIDs :: Vector Identifier)

            newChunks :: Dependency s m Chunks
            newChunks = do
                legend <- dLegend
                liftDependency $
                    foldM (applyChunkConfig legend) emptyChunks chunkConfigs

        pure $ WorldSpace wID wName initPal <$> palettes <*> pure loadPnt <*> pure shouldSerializePosition <*> pure variables <*> wScript <*> entities <*> items <*> newChunks <*> pure worldInvId
    parseJSON e = typeMismatch "WorldSpace" e

-- | When we serialize the WorldSpace, we serialize the fields in the following order
worldspaceFieldOrder :: FieldOrder
worldspaceFieldOrder = mkFieldOrderFromList
    [ "world-id"
    , "name"
    , "script"
    , "initial-palette"
    , "palettes"
    , "load-point"
    , "should-save-exact-position"
    , "inventory-id"
    , "variables"
    , "legend"
    , "chunks"
    , "entities"
    , "items"
    -- Sub Configs
    , "entity"
    , "item"
    , "position"
    , "data"
    , "emitting-signal"
    , "receiving-signal"
    , "config"
    , "file"
    , "events"
    , "is-singleton"
    , "type"
    , "value"
    , " "
    ]