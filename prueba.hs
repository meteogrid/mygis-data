{-# LANGUAGE OverloadedStrings, LiberalTypeSynonyms #-}

import Data.Time.Clock
import MyGIS.Data


main = do
    let ctx      = Context "pen_horarias_500_utm30" (Box 0 0 10 10) (Shape 100 100) ""
        st       = toAnyStore $ RasterStore "temperatura" ctx (ObservationTimeDimension vt)
        Right vt = mkSchedule "*/2 * 3 * 4,5,6"
    t <- getCurrentTime
    let ix      = ObservationTimeIx  . mkTime $ t
        Right h = mkHorizon 1
        ix2     = ForecastTimeIx (mkTime t) h
        (Just b)= fromAnyStore st :: Maybe (RasterStore ObservationTimeDimension)
        src     = getSource b ix

    print st
    print (dim b)