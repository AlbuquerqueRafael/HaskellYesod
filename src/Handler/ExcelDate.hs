module Handler.ExcelDate (stringToDate) where

import Data.Time
import Data.Time.Calendar (Day, fromGregorian, addDays, diffDays)
import Data.Time.Clock (UTCTime (UTCTime), picosecondsToDiffTime, diffTimeToPicoseconds)
    
origin :: Day
origin = fromGregorian 1899 12 30

timeFormat = "%d/%m/%Y %H:%M:%S"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat
    
dateFromDouble :: RealFrac t => t -> UTCTime
dateFromDouble d = UTCTime day diffTime
    where
      (numberOfDays, fractionOfOneDay) = properFraction d
      day = addDays numberOfDays origin
      diffTime = picosecondsToDiffTime (round (fractionOfOneDay * 24*60*60*1E12))
    
dateToDouble :: Fractional a => UTCTime -> a
dateToDouble (UTCTime day diffTime) = numberOfDays + fractionOfOneDay
    where
      numberOfDays = fromIntegral (diffDays day origin)
      fractionOfOneDay =  fromIntegral (diffTimeToPicoseconds diffTime) / (24*60*60*1E12)

stringToDate :: String -> UTCTime
stringToDate date = understandTime date