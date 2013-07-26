{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyGIS.Data.Store.Generation (
    Generation
  , GenEnv (..)
  , GenState (..)
  , GenError (..)
  , Registry

  , mkEnvironment

  , runGen
  , evalGen

  , getTime

  , noMsg
  , strMsg
  , throwError
  , catchError

  , emptyRegistry
  , lookupContext
  , lookupStore
  , registerContext
  , registerStore
  , registerContexts
  , registerStores

  , liftIO
) where


import           Control.Monad.Reader hiding (liftIO)
import           Control.Monad.State hiding (liftIO)
import           Control.Monad.Error hiding (liftIO)
import qualified Data.Map as M
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           MyGIS.Data.Store.Types

mkEnvironment :: Maybe UTCTime -> Maybe Registry -> IO (GenEnv)
mkEnvironment t r = do
  defaultTime <- getCurrentTime
  return GenEnv {
      currentTime = maybe defaultTime   id t
    , registry    = maybe emptyRegistry id r
  }

emptyRegistry :: Registry
emptyRegistry = Registry M.empty M.empty
runGen :: GenEnv -> GenState -> Generation a -> IO (Either GenError a, GenState)
runGen e s (Generation g) = runStateT (runErrorT (runReaderT g e)) s

evalGen  :: GenEnv -> GenState -> Generation a -> IO (Either GenError a)
evalGen e s g = runGen e s g >>= return . fst

liftIO :: IO a -> Generation a
liftIO = Generation . lift . lift . lift


getTime :: Generation UTCTime
getTime = asks currentTime

lookupStore :: StoreID -> Generation Store
lookupStore = lookupIn stores

registerStore :: Registry -> Store -> Maybe Registry
registerStore r s
  | M.member k sr = Nothing
  | otherwise     = Just r'
  where
    k  = storeId s
    sr = stores r
    r' = r { stores = M.insert k s sr }

registerStores :: Registry -> [Store] -> Maybe Registry
registerStores = foldMaybes registerStore

registerContext :: Registry -> Context -> Maybe Registry
registerContext r c
  | M.member k cr = Nothing
  | otherwise     = Just r'
  where
    k  = contextId c
    cr = contexts r
    r' = r { contexts = M.insert k c cr}

registerContexts :: Registry -> [Context] -> Maybe Registry
registerContexts = foldMaybes registerContext

foldMaybes :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybes fun r (a:as) =
  case fun r a of
    Nothing -> Nothing
    Just r' -> foldMaybes fun r' as
foldMaybes fun r [] = Just r

lookupContext :: ContextID -> Generation Context
lookupContext = lookupIn contexts

lookupIn :: (Ord k, Show k) => (Registry -> M.Map k v) -> k -> Generation v
lookupIn f k = do
    maybeV <- asks $ M.lookup k . f . registry
    case maybeV of
      Nothing -> throwError $ RegistryLookupError $ show k
      Just v  -> return v
