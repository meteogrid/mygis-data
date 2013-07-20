{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module MyGIS.Data.ThirdPartyInstances where

import           Data.Data
import           System.Cron

deriving instance Typeable CronSchedule
deriving instance Typeable MinuteSpec
deriving instance Typeable HourSpec
deriving instance Typeable MonthSpec
deriving instance Typeable DayOfMonthSpec
deriving instance Typeable DayOfWeekSpec
deriving instance Typeable CronField

deriving instance Data CronSchedule
deriving instance Data MinuteSpec
deriving instance Data HourSpec
deriving instance Data MonthSpec
deriving instance Data DayOfMonthSpec
deriving instance Data DayOfWeekSpec
deriving instance Data CronField
