{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SIGyM.Store.Types (
    IsStore (..)
  , Store (..)
  , Context (..)
  , ContextID
  , Registry (..)
  , StoreRegistry
  , ContextRegistry
  , StoreID (..)
  , Generation (..)
  , GenError (..)
  , GenEnv (..)
  , GenState (..)

  , fromStore
  , storeId
  , storeType
) where

import           Control.Monad.Reader (ReaderT, MonadReader)
import           Control.Monad.State (StateT, MonadState)
import           Control.Monad.Error (ErrorT, MonadError, Error(..))
import           Data.Hashable (Hashable(..))
import           Data.HashMap (Map)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable (Typeable, TypeRep, cast, typeOf)
import           SIGyM.Dimension (IsDimension(..), DimIx)
import           SIGyM.Units (Unit)
import           SIGyM.GeoReference (GeoReference)


data Context = Context
  { contextId        :: ContextID
  , geoRef           :: GeoReference
} deriving (Eq, Show, Typeable)


newtype ContextID = ContextID Text deriving (Eq,Ord,Show,Typeable,Hashable)
newtype StoreID = StoreID Text deriving (Eq,Ord,Show,Typeable,Hashable)

class ( IsDimension d
      , Eq (st d u t)
      , Show (st d u t)
      , Typeable (st d u t)
      ) =>  IsStore st d u t
  where
    type Src st d u t :: *

    sId        :: st d u t -> StoreID
    getSource  :: st d u t -> Context -> DimIx d -> Src st d u t
    getSources :: st d u t -> Context -> DimIx d -> DimIx d -> [Src st d u t]
    dimension  :: st d u t -> d
    units      :: st d u t -> Unit u t
    toStore    :: st d u t -> Store

    toStore s = Store (sId s) (typeOf s) s

    getSources s ctx from to =
        map (getSource s ctx) (enumFromToIx (dimension s) from to)

fromStore :: IsStore st d u t => Store -> Maybe (st d u t)
fromStore (Store _ _ s) = cast s


storeId :: Store -> StoreID
storeId (Store i _ _) = i

storeType :: Store -> TypeRep
storeType (Store _ t _) = t


data Store where
  Store :: IsStore st d u t => StoreID -> TypeRep -> st d u t -> Store

deriving instance Show Store
deriving instance Typeable Store
instance Hashable Store where
  hashWithSalt s (Store a _ _) = hashWithSalt s a
  {-# INLINE hashWithSalt #-}
instance Eq Store where
  (Store a _ _) == (Store b _ _) = a == b
  {-# INLINE (==) #-}
instance Ord Store where
  (Store a _ _) `compare` (Store b _ _) = a `compare` b
  {-# INLINE compare #-}


newtype Generation a = Generation
    ( ReaderT GenEnv (ErrorT GenError (StateT GenState IO)) a )
  deriving
    ( MonadError GenError, MonadState GenState, MonadReader GenEnv
    , Monad, Functor )

data GenError = OtherError String
              | RegistryLookupError String
  deriving (Show, Eq)

instance Error GenError where
  noMsg  = OtherError "Unspecified generation error"
  strMsg = OtherError


data GenState = GenState

data GenEnv = GenEnv {
    currentTime :: UTCTime
  , registry    :: Registry
}

data Registry = Registry {
    stores   :: StoreRegistry
  , contexts :: ContextRegistry
}

type StoreRegistry = Map StoreID Store
type ContextRegistry = Map ContextID Context
