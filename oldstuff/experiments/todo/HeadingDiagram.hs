{-# LANGUAGE OverloadedStrings #-}
module HeadingDiagram where

import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C
import qualified Data.Colour.Palette.ColorSet as P
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock
import Lucid.Svg hiding (translate)
import Data.OrgMode.Parse.Types

import Axis
import Shadow

hBoxHeight :: Int
hBoxHeight = 70
hBoxSep :: Int
hBoxSep    = 10

headingBackColor h = case priority h of
  Just A  -> P.d3Colors2 P.Dark 3
  Just B  -> P.d3Colors2 P.Dark 6
  Just C  -> P.d3Colors2 P.Dark 1
  Nothing -> P.d3Colors2 P.Dark 2

trimColor h = C.darken 0.5 (headingBackColor h)

clockBoxColor = C.withOpacity C.white 0.2

gTranslate :: Int -> Int -> [Attribute]
gTranslate x y = [transform_ $ T.concat ["translate(", showT x,",", showT y,")"]]

------------------------------------------------------------------------------
headingDiagram :: XAxisConfig -> Int -> Heading -> MySvg
headingDiagram axis i h =
  with g_ (gTranslate 0 (hBoxSep + (succ i)*(hBoxHeight+hBoxSep))) $ do
    dropShadow 6 6 2 (headingBackRect axis h)
    mconcat clockDiagrams
    text_ [x_ "50", y_ "50"] "Test text" -- (title h)
    headingBackTrim axis h
      where clockDiagrams :: [Svg ()]
            clockDiagrams = map (clockBlock axis h)
                            . catMaybes
                            . map fst
                            . sectionClocks
                            . section $ h

headingBackRect :: XAxisConfig -> Heading -> MySvg
headingBackRect axis h =
  with (timeRectMay axis (headingTimes h)) [
    height_ (showT hBoxHeight)
    , fill_ (T.pack . C.sRGB24show $ headingBackColor h)
    ]

headingBackTrim :: XAxisConfig -> Heading -> MySvg
headingBackTrim axis h =
  let toX     = tToX axis
      (t0,t1) = headingTimes h
      x0      = maybe (tStart axis) id t0
      x1      = maybe (tEnd   axis) id t1
      oneLine = line_ [x1_ (showF (toX x0 + 20)), x2_ (showF (toX x1 - 20))
                      , stroke_ (T.pack . C.sRGB24show $ trimColor h)]
  in do
     with oneLine [ y1_ "10"
                  , y2_ "10"]
     with oneLine [ y1_ (showT ((hBoxHeight `div` 2) - 10))
                  , y2_ (showT ((hBoxHeight `div` 2) - 10))]


clockBlock :: XAxisConfig -> Heading -> Timestamp -> MySvg
clockBlock axis h (Timestamp dateTime0 _ Nothing) =
  let x = tToX axis (dateTimeToUTC dateTime0)
      lineX = showF x
  in line_ [x1_ lineX, x2_ lineX, y1_ "-10", y2_ "10"] -- TODO Fix this
clockBlock axis h (Timestamp dateTime0 _ (Just dateTime1)) =
  with (timeRect axis (dateTimeToUTC dateTime0) (dateTimeToUTC dateTime1))
  [height_ (showT hBoxHeight)
  , fill_ (T.pack . C.sRGB24show $ clockBoxColor `C.over` headingBackColor h)]
