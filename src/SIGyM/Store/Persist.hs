{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SIGyM.Store.Persist (
    StoreLoadEnv
  , queryAllStores
  , foldAllStores
) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Text (Text)
import           Data.Maybe (fromJust)
import           Data.ByteString as BS (ByteString, concat)
import           Data.ByteString.Char8 (pack)
import           Database.PostgreSQL.Simple ( Connection, Query, fold )
import           Database.PostgreSQL.Simple.FromRow(FromRow (..), field)
import           System.FilePath (joinPath)

import           SIGyM.Store.Types
import           SIGyM.Database (sql)
import           SIGyM.DynLoad ( EvalEnv(..), EitherSymbol, loadSymbolFromModule
                               , loadSymbolFromBuffer )

allStores :: Query
allStores =  [sql|
  SELECT (type, id, storeId, md5 , fs_path, fs_module, fs_symbol , db_code)
  FROM all_stores
|]

data StoreLoadEnv = StoreLoadEnv {
    basePath :: FilePath
 ,  libdir   :: FilePath
} deriving (Eq, Show)

data StoreRow = StoreRow {
    rowType     :: !Text
  , rowId       :: !Int
  , rowStoreId  :: !Text
  , rowMD5      :: !Text
  , rowFsPath   :: !(Maybe String)
  , rowFsModule :: !(Maybe String)
  , rowFsSymbol :: !(Maybe String)
  , rowDbCode   :: !(Maybe ByteString)
  } 

data StoreRowType = FSStore | DBStore deriving (Eq, Show)

instance FromRow StoreRow where
  fromRow = StoreRow <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field


foldAllStores :: StoreLoadEnv -> Connection -> a -> (a -> Store -> IO a) -> IO a
foldAllStores env conn acc fun = fold conn allStores () acc fun'
  where fun' a r = do
           s' <- rowToStore env r
           case s' of
             Right s -> fun a s
             _       -> return a

queryAllStores :: StoreLoadEnv -> Connection -> IO [Store]
queryAllStores env conn = foldAllStores env conn [] buildList
  where buildList a s = a `seq` return (s:a)


rowToStore :: StoreLoadEnv -> StoreRow -> IO (EitherSymbol Store)
rowToStore env s@StoreRow{..} =
  case rowType of
    "fs" -> loadFSStore env s
    "db" -> loadDBStore env s
    _    -> return $ Left "Unexpected 'type' field in database row"

loadDBStore :: StoreLoadEnv -> StoreRow -> IO (EitherSymbol Store)
loadDBStore StoreLoadEnv{..} StoreRow{..} =
    loadSymbolFromBuffer env modname "store" buffer
  where env      = EvalEnv libdir []
        modname  = "DatabaseModule" ++ show rowId
        buffer   = BS.concat ["module ", pack modname, "(store) where\n", code]
        code     = fromJust rowDbCode


loadFSStore :: StoreLoadEnv -> StoreRow -> IO (EitherSymbol Store)
loadFSStore StoreLoadEnv{..} StoreRow{..} =
    loadSymbolFromModule env modname symbol
  where iPath   = joinPath [basePath, (fromJust rowFsPath)]
        env     = EvalEnv libdir [iPath]
        modname = fromJust rowFsModule
        symbol  = fromJust rowFsSymbol
