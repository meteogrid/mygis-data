{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyGIS.Data.Store.Generation (
    Generation
  , GenEnv (..)
  , GenState (..)
  , GenError (..)

  , mkEnvironment

  , runGen
  , evalGen

  , getTime

  , noMsg
  , strMsg
  , throwError
  , catchError

  , liftIO
) where


import           Control.Monad.Reader hiding (liftIO)
import           Control.Monad.State hiding (liftIO)
import           Control.Monad.Error hiding (liftIO)
import qualified Data.Map as M
import           Data.Time.Clock (UTCTime, getCurrentTime)

import           MyGIS.Data.Store.Types

mkEnvironment :: IO (GenEnv)
mkEnvironment = do
  t <- getCurrentTime
  return GenEnv {
      currentTime = t
    , registry = Registry M.empty M.empty
  }

runGen :: GenEnv -> GenState -> Generation a -> IO (Either GenError a, GenState)
runGen e s (Generation g) = runStateT (runErrorT (runReaderT g e)) s

evalGen  :: GenEnv -> GenState -> Generation a -> IO (Either GenError a)
evalGen e s g = runGen e s g >>= return . fst

liftIO :: IO a -> Generation a
liftIO = Generation . lift . lift . lift


getTime :: Generation UTCTime
getTime = asks currentTime

lookupStore :: StoreID -> Generation (Maybe Store)
lookupStore = lookupIn stores

lookupContext :: ContextID -> Generation (Maybe Context)
lookupContext = lookupIn contexts

lookupIn :: Ord k => (Registry -> M.Map k v) -> k -> Generation (Maybe v)
lookupIn f k = asks (M.lookup k . f . registry)
