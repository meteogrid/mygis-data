{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

import Data.Time.Clock
import Data.Maybe
import qualified Control.Monad.Error as E
import MyGIS.Data
import MyGIS.Data.Units

import Numeric.Units.Dimensional.TF.SIUnits (meter)
import Numeric.Units.Dimensional.TF


main = do
    let Right ctx = mkContext "pen_horarias_500_utm30" (mkEnvelope 0 0 10 10) (mkShape 10 10) ""
        rs :: RasterStore ObservationTimeDimension RealLength
        rs       = RasterStore (mkType "altura") ctx (ObservationTimeDimension vt) meter
        st        = toAnyStore rs
        Right vt = mkSchedule "*/2 * 3 * 4,5,6"
    t <- getCurrentTime
    let ix      = ObservationTimeIx  . mkTime $ t
        Right h = mkHorizon 1
        b :: RasterStore ObservationTimeDimension RealLength
        ix2     = ForecastTimeIx (mkTime t) h
        (Just b)= fromAnyStore st
        src :: Src RasterStore ObservationTimeDimension RealLength
        src     = getSource b ix
    print st
    print (dimension b)
    print src

