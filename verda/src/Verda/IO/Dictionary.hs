module Verda.IO.Dictionary where

import           Control.Lens
import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.Map               as Map

import           Verda.IO.Directory     (WorkingDirectory)
import           Verda.IO.Files         (loadFromFileDP, parseAllDP)
import           Verda.Util.Dependency
import           Verda.Util.Types

type DictionaryLoader m d s = FilePath -> (d, s) -> m (d, s)

parseFiles :: (s `CanStore` WorkingDirectory, FromJSON (Dependency s m a), MonadIO m) => Setter' d (IDMap a) -> String -> (a -> Identifier) -> DictionaryLoader m d s
parseFiles updateDict extension getID assetsDir (dict, parsingStore) = do
    depValues        <- liftIO $ parseAllDP extension assetsDir
    (values, store') <- flip runDependency parsingStore . flattenDependency $ depValues
    let idMap = Map.fromList . fmap (\v -> (getID v, v)) $ values
        dict' = (updateDict .~ idMap) dict
    pure (dict', store')

parseUnique :: (s `CanStore` WorkingDirectory, FromJSON (Dependency s m a), MonadIO m) => Setter' d a -> String -> DictionaryLoader m d s
parseUnique updateDict filePath assetsDir (dict, parsingStore) = do
    depValue <- liftIO $ loadFromFileDP (assetsDir ++ "/" ++ filePath)
    (value, store') <- runDependency depValue parsingStore 
    let dict' = (updateDict .~ value) dict
    pure (dict', store')

runDictionaryLoaders :: MonadIO m => String -> d -> s -> [DictionaryLoader m d s] ->  m (d, s)
runDictionaryLoaders assetsDir initDict initStore = foldM nextLoader (initDict, initStore)
    where nextLoader acc loader = loader assetsDir acc