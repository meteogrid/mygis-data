{-# LANGUAGE OverloadedStrings, LiberalTypeSynonyms #-}

import Data.Time.Clock
import MyGIS.Data
import System.Cron.Parser
import Data.Attoparsec.Text (parseOnly)


main = do
    let ctx      = Context "pen_horarias_500_utm30" (Box 0 0 10 10) (Shape 100 100) ""
        st       = RasterStore "temperatura" ctx (ObservationTimeDimension vt)
        Right vt = parseOnly cronSchedule "*/2 * 3 * 4,5,6"
    t <- getCurrentTime
    let ix  = ObservationTimeIx  . mkTime $ t
        src = RasterSource ix

    print (dimIx . toSource $ src)
