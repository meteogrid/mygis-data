{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import           SIGyM.Database (sql)

allStores :: Query
allStores =  [sql|
  SELECT (type, id, storeId, md5 , fs_path, fs_module, fs_symbol , db_code)
  FROM all_stores
|]

data LoadEnv = LoadEnv {
    basePath :: FilePath
 ,  libdir   :: FilePath
} deriving (Eq, Show)

data StoreRow = StoreRow {
    rowType     :: !Text
  , rowId       :: !Int
  , rowStoreId  :: !Text
  , rowMD5      :: !Text
  , rowFsPath   :: !(Maybe Text)
  , rowFsModule :: !(Maybe Text)
  , rowFsSymbol :: !(Maybe Text)
  , rowDbCode   :: !(Maybe Text)
  } 

instance FromRow StoreRow where
  fromRow = StoreRow <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field



foldAllStores :: Connection -> a -> (a -> Store -> IO a) -> IO a
foldAllStores conn acc fun = fold conn allStores () acc fun'
  where fun' a = fun a . rowToStore

queryAllStores :: Connection -> IO [Store]
queryAllStores conn = liftM (map rowToStore) $ query conn allStores ()

rowToStore :: StoreRow -> Store
rowToStore s@StoreRow{..} =
  case rowType of
    "fs" -> undefined
