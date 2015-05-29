module HeadingDiagram where

import Data.Maybe
import Data.Time
import Data.Time.Clock
import Diagrams.Prelude hiding (section)
import Diagrams.Backend.SVG
import Lucid.Svg hiding (translate)
import Data.OrgMode.Parse.Types

import Axis

hBoxHeight :: Double
hBoxHeight = 70

headingBackColor = fromAlphaColour $ withOpacity green 1

trimColor = fromAlphaColour $ withOpacity white 0.5

clockBoxColor = fromAlphaColour $ withOpacity white 0.25

------------------------------------------------------------------------------
headingDiagram :: Int -> XAxisConfig -> Heading -> HeadingDiagram
headingDiagram _ axis h =
  headingBackTrim axis h <> mconcat clockDiagrams <> headingBackRect axis h
  where clockDiagrams :: [HeadingDiagram]
        clockDiagrams = map (clockBlock axis)
                        . catMaybes
                        . map fst
                        . sectionClocks
                        . section $ h

headingBackRect :: XAxisConfig -> Heading -> HeadingDiagram
headingBackRect axis h =
  timeRectMay axis (headingTimes h)
  # scaleY hBoxHeight
  # fc   headingBackColor

headingBackTrim :: XAxisConfig -> Heading -> HeadingDiagram
headingBackTrim axis h =
  let toX     = tToX axis
      (t0,t1) = headingTimes h
      x0      = maybe (tStart axis) id t0
      x1      = maybe (tEnd   axis) id t1
      oneLine = ((P $ V2 (toX (maybe (tStart axis) id t0)) 0)
                 ~~
                 (P $ V2 (toX (maybe (tEnd axis) id t1)) 0))
                 # strokeP
  in (oneLine # translateY (hBoxHeight / 2 - 5)
      `atop`
      oneLine # translateY (5 - hBoxHeight / 2))
     # lc trimColor

clockBlock :: XAxisConfig -> Timestamp -> HeadingDiagram
clockBlock axis (Timestamp dateTime0 _ Nothing) =
  let x = tToX axis (dateTimeToUTC dateTime0)
  in  ((P $ V2 x (hBoxHeight/2)) ~~ (P $ V2 x (negate $ hBoxHeight/2)))
      # strokeP
clockBlock axis (Timestamp dateTime0 _ (Just dateTime1)) =
  timeRect axis (dateTimeToUTC dateTime0) (dateTimeToUTC dateTime1)
  # scaleY hBoxHeight
  # fc clockBoxColor
