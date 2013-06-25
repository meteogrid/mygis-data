{-# LANGUAGE GADTs #-}

module MyGIS.Data.Source (
    Source (Source),
    IsSource (..),
    RasterSource (RasterSource)
) where


import           Data.Text (Text)
import           MyGIS.Data.Context (Context)
import           MyGIS.Data.Dimension (DimensionIx)

data RasterSource = RasterSource {
    rsName    :: Text
  , rsContext :: Context
  , rsDimIx   :: DimensionIx
} deriving (Eq, Show)

class IsSource source where
    name    :: source -> Text
    context :: source -> Context
    dimIx   :: source -> DimensionIx

instance IsSource RasterSource where
    name    = rsName
    context = rsContext
    dimIx   = rsDimIx

data Source where
    Source :: IsSource s => s -> Source
