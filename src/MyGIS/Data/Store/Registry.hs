{-# LANGUAGE RecordWildCards #-}
module MyGIS.Data.Store.Registry (
    Registry

  , emptyRegistry
  , lookupContext
  , lookupStore
  , registerContext
  , registerStore
  , registerContexts
  , registerStores
) where

import qualified Data.HashMap as M
import           MyGIS.Data.Store.Types

emptyRegistry :: Registry
emptyRegistry = Registry M.empty M.empty

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

lookupContext :: ContextID -> Registry -> Maybe Context
lookupContext k Registry{..} = M.lookup k contexts

lookupStore :: StoreID -> Registry -> Maybe Store
lookupStore k Registry{..} = M.lookup k stores



foldMaybes :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybes fun r (a:as) =
  case fun r a of
    Nothing -> Nothing
    Just r' -> foldMaybes fun r' as
foldMaybes _ r [] = Just r
