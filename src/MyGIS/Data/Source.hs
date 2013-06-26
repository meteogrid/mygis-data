{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MyGIS.Data.Source (
    IsSource (..)
  , Source (Source)
  , RasterSource (RasterSource)

  , dimIx
) where


class IsSource s ix u where
    toSource :: s -> Source ix u

data Source ix u = Source {
   dimIx :: ix
 , units :: u
}  deriving (Eq, Show)

data RasterSource ix u = RasterSource {
   rsDimIx   :: ix
 , rsUnits   :: u
}  deriving (Eq, Show)

instance IsSource (RasterSource ix u) ix u where
    toSource rs = Source (rsDimIx rs) (rsUnits rs)
