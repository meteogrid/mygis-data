{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module SIGyM.Store.Types (
    Dimension (..)
  , Dimensions
  , DimensionIx
  , DynUnits
  , Ix (..)
  , Store (..)
  , StoreID (..)
  , Context (..)
  , ContextID
  , Registry (..)
  , StoreRegistry
  , ContextRegistry
  , Generation (..)
  , GenError (..)
  , GenEnv (..)
  , GenState (..)

  , storeId
  , storeDimensions
  , storeUnits
  , storeUnitsType

  , toDynUnits
) where

import           Control.Monad.Reader (ReaderT, MonadReader)
import           Control.Monad.State (StateT, MonadState)
import           Control.Monad.Error (ErrorT, MonadError, Error(..))
import           Data.Char (isAlphaNum, isAscii)
import           Data.Dynamic (Dynamic, fromDynamic, dynTypeRep, toDyn)
import           Data.Hashable (Hashable(..))
import           Data.HashMap (Map)
import           Data.String (IsString(..))
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable (Typeable, TypeRep)
import           SIGyM.Units (Unit)
import           SIGyM.GeoReference (GeoReference)
import           SIGyM.Time
import           SIGyM.ThirdPartyInstances()

type Dimensions = [Dimension]
type DimensionIx = [Ix]

data Dimension = ObservationTimeDim CronSchedule
               | RunTimeDim CronSchedule
               | HorizonDim Horizon
               deriving (Eq, Show, Typeable)

data Ix = ObservationTimeIx Time
        | RunTimeIx Time
        | HorizonIx Horizon
        deriving (Eq, Ord, Show, Typeable)


data Context = Context
  { contextId        :: ContextID
  , geoRef           :: GeoReference
} deriving (Eq, Show, Typeable)

class IsID a where
   toID :: String -> Maybe a

newtype ContextID = ContextID Text deriving (Eq,Ord,Show,Typeable,Hashable)

instance IsString ContextID where
  fromString s = maybe (error "fromString: invalid ContextID") id (toID s)

instance IsID ContextID where
  toID s = if isValidId s then Just $ ContextID $ pack s else Nothing


newtype StoreID = StoreID Text deriving (Eq,Ord,Show,Typeable,Hashable)

instance IsString StoreID where
  fromString s = maybe (error "fromString: invalid StoreID") id (toID s)

instance IsID StoreID where
  toID s = if isValidId s then Just $ StoreID $ pack s else Nothing


isValidId :: String -> Bool
isValidId s =  all isValidChar s
            && head s `notElem` validNonAlphanum
            && length s < 255
            && length s > 0
  where isValidChar c =  all (\f -> f c) [isAlphaNum, isAscii]
                      || c `elem` validNonAlphanum
        validNonAlphanum = "-_."

newtype DynUnits = DynUnits { unDyn :: Dynamic } deriving (Typeable, Show)

toDynUnits :: Typeable (Unit a Double) => Unit a Double -> DynUnits
toDynUnits = DynUnits . toDyn

data Store
  = RasterStore {
        rsId         :: StoreID
      , rsDimensions :: Dimensions
      , rsUnits      :: DynUnits
    }
  | GeometryStore {
        vsId         :: StoreID
      , vsDimensions :: Dimensions
      , vsUnits      :: DynUnits
  }
  deriving (Show, Typeable)

instance Hashable Store where
  hashWithSalt s st = hashWithSalt s (storeId st)
  {-# INLINE hashWithSalt #-}

instance Eq Store where
  a == b = storeId a == storeId b
  {-# INLINE (==) #-}
instance Ord Store where
  a `compare` b = storeId a `compare` storeId b
  {-# INLINE compare #-}

storeId :: Store -> StoreID
storeId RasterStore{..} = rsId
storeId GeometryStore{..} = vsId

storeDimensions :: Store -> Dimensions
storeDimensions RasterStore{..} = rsDimensions
storeDimensions GeometryStore{..} = vsDimensions

storeUnits :: Typeable (Unit d a) => Store -> Maybe (Unit d a)
storeUnits RasterStore{..} = fromDynamic $ unDyn rsUnits
storeUnits GeometryStore{..} = fromDynamic $ unDyn vsUnits

storeUnitsType :: Store -> TypeRep
storeUnitsType RasterStore{..} = dynTypeRep $ unDyn rsUnits
storeUnitsType GeometryStore{..} = dynTypeRep $ unDyn vsUnits



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
