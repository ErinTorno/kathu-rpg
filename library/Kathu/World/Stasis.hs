module Kathu.World.Stasis where

import           Apecs                     hiding (Get, get)
import           Data.Int
import qualified Data.Map                  as Map
import           Data.Serialize
import qualified Data.Vector.Unboxed       as UVec
import           Linear.V2                 (V2(..))

import           Kathu.Scripting.Variables
import           Verda.Util.Types          (Identifier, IDMap)

------------
-- Stasis --
------------

-- | An object describing saved state information about a WorldSpace while it is inactive
data WorldStasis = WorldStasis
    { statisVariables  :: IDMap WorldVariable -- The world's variables
    -- Where the player entered the world from so we can reset them back to it
    -- Optionally, the player's actual location if the world supports it
    , playerEntryPoint :: !(V2 Double)
    , removedEntities  :: UVec.Vector Int32     -- The index IDs of entities that are to be removed from the world's entity vector before spawning
    , removedItems     :: UVec.Vector Int32     -- Same but for items
    --, leftItems       :: ?
    }

instance Semigroup WorldStases where (<>) = mappend
instance Monoid WorldStases where mempty = WorldStases Map.empty
instance Component WorldStases where type Storage WorldStases = Global WorldStases

emptyWorldStasis :: WorldStasis
emptyWorldStasis = WorldStasis Map.empty (V2 0 0) UVec.empty UVec.empty

worldStasisVersion :: Int32
worldStasisVersion = -1 -- unstable, no guarantee of backwards compatibility until >= 0

instance Serialize WorldStasis where
    put (WorldStasis vars ep remEtys remItems) = put worldStasisVersion >> put vars >> put ep >> put remEtys >> put remItems
    get = (get :: Get Int32) >>= ver
        where ver v | v == worldStasisVersion = WorldStasis <$> get <*> get <*> get <*> get
                    | otherwise               = fail ("Unknown WorldStasis Version " ++ show v)

------------
-- Stases --
------------

-- | A global component used for accessing the stases of hibernating WorldSpaces
newtype WorldStases = WorldStases {unStases :: IDMap WorldStasis} deriving Serialize

updateStasis :: Identifier -> WorldStases -> (WorldStasis -> WorldStasis) -> WorldStases
updateStasis idt (WorldStases w) f = WorldStases . Map.alter applyF idt $ w
    where applyF Nothing  = Just $ f emptyWorldStasis
          applyF (Just s) = Just $ f s

getStasis :: Identifier -> WorldStases -> Maybe WorldStasis
getStasis idt = Map.lookup idt . unStases