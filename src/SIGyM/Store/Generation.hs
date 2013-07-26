{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SIGyM.Store.Generation (
    Generation
  , GenEnv (..)
  , GenState (..)
  , GenError (..)
  , Registry

  , mkEnvironment

  , runGen
  , evalGen

  , getTime
  , findStore
  , findContext

  , noMsg
  , strMsg
  , throwError
  , catchError

  , liftIO
) where


import           Control.Monad.Reader hiding (liftIO)
import           Control.Monad.State hiding (liftIO)
import           Control.Monad.Error hiding (liftIO)
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           SIGyM.Store.Registry
import           SIGyM.Store.Types

mkEnvironment :: Maybe UTCTime -> Maybe Registry -> IO (GenEnv)
mkEnvironment t r = do
  defaultTime <- getCurrentTime
  return GenEnv {
      currentTime = maybe defaultTime   id t
    , registry    = maybe emptyRegistry id r
  }

runGen :: GenEnv -> GenState -> Generation a -> IO (Either GenError a, GenState)
runGen e s (Generation g) = runStateT (runErrorT (runReaderT g e)) s

evalGen  :: GenEnv -> GenState -> Generation a -> IO (Either GenError a)
evalGen e s g = runGen e s g >>= return . fst

liftIO :: IO a -> Generation a
liftIO = Generation . lift . lift . lift


getTime :: Generation UTCTime
getTime = asks currentTime

findStore :: StoreID -> Generation Store
findStore k = do
  mv <- asks (lookupStore k . registry)
  maybe (throwError$ RegistryLookupError$ "No such store: "++show k) return mv

findContext :: ContextID -> Generation Context
findContext k = do
  mv <- asks (lookupContext k . registry)
  maybe (throwError$ RegistryLookupError$ "No such context: "++show k) return mv
