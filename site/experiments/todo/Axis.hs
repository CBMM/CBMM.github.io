{-# LANGUAGE RecordWildCards #-}

module Axis where

import qualified Data.HashMap.Strict as HM
import Data.Time
import Data.Time.Clock
import Diagrams.Prelude hiding (section)
import Diagrams.Backend.SVG
import Lucid.Svg hiding (translate)
import Data.OrgMode.Parse.Types

type HeadingDiagram = QDiagram SVG V2 Double Any

timeRect :: XAxisConfig -> UTCTime -> UTCTime -> HeadingDiagram
timeRect axis@XAxisConfig{..} t0 t1 =
  let [x0,x1] = map (tToX axis) [t0,t1] :: [Double]
  in  rect (x1 - x0) 1 & translate (V2 x0 0)

timeRectMay :: XAxisConfig
            -> (Maybe UTCTime,Maybe UTCTime)
            -> HeadingDiagram
timeRectMay axis@XAxisConfig{..} (mT0, mT1) =
  timeRect axis (maybe tStart id mT0) (maybe tEnd id mT1)

data XAxisConfig = XAxisConfig {
  tStart   :: UTCTime
  , tEnd   :: UTCTime
  , tFocus :: UTCTime
  , xStart :: Double
  , xEnd   :: Double
  , zFrac  :: Double
  }

xAxis0 :: XAxisConfig
xAxis0 = XAxisConfig {
  tStart = UTCTime (fromGregorian 2014 5 1) 0
  , tEnd = UTCTime (fromGregorian 2017 1 1) 0
  , xStart = 10
  , xEnd   = 510
  , tFocus  = UTCTime (fromGregorian 2015 5 20) 0
  , zFrac = 0.2
  }

tToX :: XAxisConfig -> UTCTime -> Double
tToX XAxisConfig{..} t =
  let m = (xEnd - xStart) /
          realToFrac (diffUTCTime tEnd tStart) :: Double
      xNoZoom   = (realToFrac $ diffUTCTime t tStart) * m +
                  xStart :: Double
      xFullZoom
        | t == tFocus = xNoZoom
        | t >  tFocus = xEnd
        | otherwise   = xStart
      x = xNoZoom * (1-zFrac) + xFullZoom * zFrac
  in x

dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC (DateTime (YMD' (YearMonthDay y m d)) _ mayHourMin _ _) =
  let theHour = maybe 0 fst mayHourMin
      theMin  = maybe 0 snd mayHourMin
      secsIn  = 60*60*theHour + 60*theMin
  in  UTCTime (fromGregorian (fromIntegral y) m d) (fromIntegral secsIn)


headingTimes :: Heading -> (Maybe UTCTime, Maybe UTCTime)
headingTimes heading =
  case sectionPlannings . section $ heading of
   Plns ps -> ((dateTimeToUTC . tsTime) <$> HM.lookup SCHEDULED ps,
               (dateTimeToUTC . tsTime) <$> HM.lookup CLOSED ps)

clockTimes :: Timestamp -> (Maybe UTCTime, Maybe UTCTime)
clockTimes (Timestamp t0 _ mT1) = (dateTimeToUTC <$> Just t0,
                                   dateTimeToUTC <$> mT1)

