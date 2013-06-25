{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MyGIS.Data.Source (
    IsSource (..)
  , Source (Source)
  , RasterSource (RasterSource)

  , dimIx
) where


class IsSource s ix where
    toSource :: s -> Source ix

data Source ix = Source {
   dimIx :: ix
}  deriving (Eq, Show)

instance IsSource (Source ix) ix where
    toSource = id

data RasterSource ix = RasterSource {
   rsDimIx   :: ix
}  deriving (Eq, Show)

instance IsSource (RasterSource ix) ix where
    toSource rs = Source (rsDimIx rs)
