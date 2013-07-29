{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module SIGyM.Store.Persist (
    queryAllStores
  , foldAllStores
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (liftM)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple ( Connection, Query, fold, query )
import           Database.PostgreSQL.Simple.FromRow(FromRow (..), field)

import           SIGyM.Store.Types

allStores :: Query
allStores = "SELECT ( type, id, storeId, md5\
                   \, fs_path, fs_module, fs_symbol\
                   \, db_code )\
              \ FROM all_stores"

data StoreDAO = StoreDAO {
    daoType     :: !Text
  , daoId       :: !Int
  , daoStoreId  :: !Text
  , daoMD5      :: !Text
  , daoFsPath   :: !(Maybe Text)
  , daoFsModule :: !(Maybe Text)
  , daoFsSymbol :: !(Maybe Text)
  , daoDbCode   :: !(Maybe Text)
  } 

instance FromRow StoreDAO where
  fromRow = StoreDAO <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field



foldAllStores :: Connection -> a -> (a -> Store -> IO a) -> IO a
foldAllStores conn acc fun = fold conn allStores () acc fun'
  where fun' a = fun a . daoToStore

queryAllStores :: Connection -> IO [Store]
queryAllStores conn = liftM (map daoToStore) $ query conn allStores ()

daoToStore :: StoreDAO -> Store
daoToStore s@StoreDAO{..} =
  case daoType of
    "fs" -> undefined
