module HeadingDiagram where

import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.Clock
import Lucid.Svg hiding (translate)
import Data.OrgMode.Parse.Types

import Axis
import Shadow

hBoxHeight :: Double
hBoxHeight = 70

headingBackColor = fromAlphaColour $ withOpacity green 1

trimColor = fromAlphaColour $ withOpacity white 1

clockBoxColor = fromAlphaColour $ withOpacity white 1

------------------------------------------------------------------------------
headingDiagram :: Int -> XAxisConfig -> Heading -> Svg ()
headingDiagram _ axis h =
  headingBackTrim axis h <> mconcat clockDiagrams <> headingBackRect axis h
  where clockDiagrams :: [HeadingDiagram]
        clockDiagrams = map (clockBlock axis)
                        . catMaybes
                        . map fst
                        . sectionClocks
                        . section $ h

headingBackRect :: XAxisConfig -> Heading -> Svg ()
headingBackRect axis h =
  with (timeRectMay axis (headingTimes h)) [
    height_ (showF hBoxHeight)
    , stroke_width_ "0"
    , fill_ "green" -- TODO Fix this.
    ]

headingBackTrim :: XAxisConfig -> Heading -> Svg ()
headingBackTrim axis h =
  let toX     = tToX axis
      (t0,t1) = headingTimes h
      x0      = maybe (tStart axis) id t0
      x1      = maybe (tEnd   axis) id t1
      --oneLine = ((P $ V2 (toX (maybe (tStart axis) id t0)) 0)
      --           ~~
      --           (P $ V2 (toX (maybe (tEnd axis) id t1)) 0))
      --           # strokeP
      oneLine = line_ [x1_ (showF x0), x2_ (showF x1), stroke_ "rgb(255,0,0)"] -- TODO Fix this
  in do
     with oneLine [y1_ (showF (hBoxHeight / 2 - 10)), y2_ (showF (hBoxHeight / 2 - 10))]
     with oneLine [y1_ (showF (10 - hBoxHeight / 2)), y2_ (showF (10 - hBoxHeight / 2 - 10))]


clockBlock :: XAxisConfig -> Timestamp -> Svg ()
clockBlock axis (Timestamp dateTime0 _ Nothing) =
  let x = tToX axis (dateTimeToUTC dateTime0)
      lineX = showF x
  in line_ [x1_ lineX, x2_ lineX, y1 "-10", y2 "10"] -- TODO Fix this

--  in  ((P $ V2 x (hBoxHeight/2)) ~~ (P $ V2 x (negate $ hBoxHeight/2)))
--      # strokeP
clockBlock axis (Timestamp dateTime0 _ (Just dateTime1)) =
  with (timeRect axis (dateTimeToUTC dateTime0) (dateTimeToUTC dateTime1))
  [height_ "70"]  -- TODO Fix this
--  timeRect axis (dateTimeToUTC dateTime0) (dateTimeToUTC dateTime1)
--  # scaleY hBoxHeight
--  # fc clockBoxColor
--  # lw none
--  # opacity 0.10
