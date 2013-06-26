{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

import Data.Time.Clock
import Data.Maybe
import MyGIS.Data

import Numeric.Units.Dimensional.TF.SIUnits (meter)
import Numeric.Units.Dimensional.TF


main = do
    let ctx      = Context "pen_horarias_500_utm30" (Box 0 0 10 10) (Shape 100 100) ""
        rs :: RasterStore ObservationTimeDimension RealLength
        rs       = RasterStore "temperatura" ctx (ObservationTimeDimension vt) meter
        st        = toAnyStore rs
        Right vt = mkSchedule "*/2 * 3 * 4,5,6"
    t <- getCurrentTime
    let ix      = ObservationTimeIx  . mkTime $ t
        Right h = mkHorizon 1
        ix2     = ForecastTimeIx (mkTime t) h
        (Just b)= fromAnyStore st :: Maybe (RasterStore ObservationTimeDimension RealLength)
        src     = getSource b ix :: RasterSource ObservationTimeIx RealLength
    print (units b) 
    print (dim b)
