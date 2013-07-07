{-# LANGUAGE DeriveDataTypeable #-}
module MyGIS.Data.Time (
    Time
  , Horizon
  , CronSchedule

  , mkTime
  , mkHorizon
  , mkHorizons
  , mkSchedule

  , addHorizon
) where

import           Data.Data (Data, Typeable)
import           Data.Time.Calendar ( Day(ModifiedJulianDay) )
import           Data.Time.Clock ( UTCTime(UTCTime), secondsToDiffTime,
                                  addUTCTime, diffUTCTime, NominalDiffTime )
import           Data.Text (Text)
import           Data.Attoparsec.Text (parseOnly)
import           Data.List (nub, sort)
import           System.Cron (CronSchedule)
import           System.Cron.Parser (cronSchedule)
import           System.Locale (defaultTimeLocale)
import qualified Data.Time.Format as T
import           MyGIS.Data.Error (EitherError, mapE, mkError)

newtype Time = Time {toUTCTime :: UTCTime} deriving (Eq, Ord, Typeable, Data)

instance Show Time where show = formatTime iso8601

formatTime :: String -> Time -> String
formatTime fmt = T.formatTime defaultTimeLocale fmt . toUTCTime

iso8601 :: String
iso8601 = "%Y-%m-%dT%H:%MZ"

mkTime :: UTCTime -> Time
mkTime = Time


addHorizon :: Time -> Horizon -> Time
addHorizon t h
    = Time $ addUTCTime (horizonToNominalDiffTime h) (toUTCTime t)

horizonToNominalDiffTime :: Horizon -> NominalDiffTime
horizonToNominalDiffTime = minutesToNominalDiffTime . fromIntegral . minutes

minutesToNominalDiffTime :: Integer -> NominalDiffTime
minutesToNominalDiffTime = secondsToNominalDiffTime . (*60)

secondsToNominalDiffTime :: Integer -> NominalDiffTime
secondsToNominalDiffTime s | s >= 0    = diffUTCTime b a
                           | otherwise = diffUTCTime a b
  where a = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        b = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime $ abs s)





newtype Horizon = Minutes {minutes :: Int}
  deriving (Eq, Ord, Show, Typeable, Data)


mkHorizon :: (Integral a, Show a) => a -> EitherError Horizon
mkHorizon m | m >= 0     = Right . Minutes . fromIntegral $ m
            | otherwise  = mkError ("mkHorizon: '"
                                ++ show m
                                ++ "' Cannot be negative")

mkHorizons :: (Integral a, Show a) => [a] -> EitherError [Horizon]
mkHorizons [] = mkError "mkHorizons: Empty list"
mkHorizons xs = let result = mapE mkHorizon "; " xs in
                case result of
                    Right hs -> Right . sort . nub $ hs
                    e        -> e


mkSchedule :: Text -> EitherError CronSchedule
mkSchedule s = let result = parseOnly cronSchedule s in
               case result of
                 Right r -> Right r
                 Left e  -> mkError e
