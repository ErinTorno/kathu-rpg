module Verda.Graphics.Fonts where

import           Apecs                   hiding (($=))
import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.ST        (RealWorld, stToIO)
import           Data.Aeson
import           Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HT
import           Data.MonoTraversable    (ofoldlM)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Foreign.C.Types         (CInt)
import           Linear.V2               (V2(..))
import           Linear.V3               (V3(..))
import           Linear.V4               (V4(..))
import           SDL                     (($=))
import qualified SDL
import qualified SDL.Font                as SDLF

import           Verda.Graphics.Color
import           Verda.IO.Directory
import           Verda.Util.Dependency
import           Verda.Util.Types        (Identifier, IDMap)

data Font = Font
    { fontID :: !Identifier
    , unFont :: SDLF.Font
    }

instance (s `CanProvide` WorkingDirectory, MonadIO m) => FromJSON (Dependency s m Font) where
    parseJSON = withObject "Font" $ \v -> do
        fontID <- v .: "font-id"
        filePath <- resolveAssetPathDP <$> v .: "file"
        size     <- v .: "size"
        pure $ fmap (Font fontID) . liftDependency . flip SDLF.load size =<< filePath

-- in future, might want to load this from language file
defaultCachedChars :: String
defaultCachedChars = [' '..'~'] -- Most letters, numbers, and symbols for American keyboard

defaultFontCacheSize :: Int
defaultFontCacheSize = 128

newtype CharCache = CharCache {unCharCache :: HashTable RealWorld Char (V2 CInt, SDL.Texture)}

data FontCache = FontCache
    { fontMap    :: !(IDMap Font)
    , charCaches :: !(HashTable RealWorld Identifier CharCache)
    }

instance Semigroup FontCache where (<>) = mappend
instance Monoid FontCache where mempty = error "Attempted to use FontCache before it has been initialized"
instance Component FontCache where type Storage FontCache = Global FontCache

writeChar :: MonadIO m => SDL.Renderer -> Font -> CharCache -> Char -> m (V2 CInt, SDL.Texture)
writeChar !renderer (Font _ !font) (CharCache !cache) !key = do
    -- nothing in cache, so we need to render the char then return texture
    surface <- SDLF.blended font (unColor white) (T.singleton key)
    size    <- SDL.surfaceDimensions surface
    texture <- SDL.createTextureFromSurface renderer surface
    liftIO . stToIO $ HT.insert cache key (size, texture)
    -- We can now freely release this, as only the texture should be used now
    SDL.freeSurface surface
    pure (size, texture)

getCharacter :: MonadIO m => SDL.Renderer -> Font -> CharCache -> Char -> m (V2 CInt, SDL.Texture)
getCharacter !renderer !font (CharCache !cache) !key = insertIfNeeded =<< (liftIO . stToIO $ HT.lookup cache key)
    where insertIfNeeded (Just p) = pure p
          insertIfNeeded Nothing  = writeChar renderer font (CharCache cache) key

-- | Renderers the given text using the given font at a position, and returns the ending x position
renderText :: MonadIO m => SDL.Renderer -> FontCache -> Identifier -> Color -> V2 CInt -> Text -> m CInt
renderText !renderer (FontCache fonts caches) !fontID (Color (V4 !r !g !b !a)) (V2 !x0 !y0) !text = do
    let -- generic error message so that we don't use the generic key-not-found one
        missingCache :: String -> a
        missingCache t = error $ "Could not find font " ++ show fontID ++ " in " ++ t ++ " cache"
        font           = fromMaybe (missingCache "font") . Map.lookup fontID $ fonts
    cache <- liftIO . stToIO $ fromMaybe (missingCache "character") <$> HT.lookup caches fontID

    let drawChar !x !ch = do
            (size@(V2 w _), texture) <- getCharacter renderer font cache ch
            SDL.textureColorMod texture $= V3 r g b
            SDL.textureAlphaMod texture $= a
            SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P (V2 x y0)) size)
            pure (x + w)

    ofoldlM drawChar x0 text
    
initFontCache :: MonadIO m => SDL.Renderer -> IDMap Font -> m FontCache
initFontCache !renderer !fonts = do
    allCaches <- liftIO . stToIO $ HT.newSized (Map.size fonts)

    forM_ (Map.assocs fonts) $ \(fontID, font) -> do
        cache <- liftIO . stToIO . fmap CharCache $ HT.newSized defaultFontCacheSize
        -- we pre-render each char as a texture and store it in the cache
        forM_ defaultCachedChars $ writeChar renderer font cache
        liftIO . stToIO $ HT.insert allCaches fontID cache
        
    pure (FontCache fonts allCaches)