{-# LANGUAGE CPP               #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Error (note)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.Text as T
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Reflex.Dom
import qualified Data.Text.Lazy       as TL
import           Lucid.Svg            hiding ((<>))

import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types

import Axis
import HeadingDiagram

--showT :: Show a => a -> T.Text
--showT = T.pack . show

--showF :: RealFrac a => a -> T.Text
--showF = T.pack . show . floor

myOrgUrl :: String
#ifdef __GHCJS__
myOrgUrl = "https://cdn.rawgit.com/CBMM/CBMM.github.io/master/site/experiments/todo.org"
#else
myOrgUrl = "file:///home/greghale/Programming/CBMM.github.io/site/experiments/todo.org"
#endif

main :: IO ()
main = mainWidget $ mdo

  Reflex.Dom.text (myOrgUrl)
  --diagramWidget "head circle" (circle 100)
  refreshClick <- button "Refresh org data"
  fetchOrgTriggers <- appendEvents refreshClick <$> getPostBuild
  -- TODO: Data doesn't change on reload after changing todo.org. why?
  orgEvents <- flip fforMaybe (either (const Nothing) Just) <$> fetchOrgFile fetchOrgTriggers
  orgData <- holdDyn (Document "No document yet" [])
             orgEvents
  el "div" $ orgWidget orgData
  el "hr" (return ())
  dynText =<< mapDyn show orgData
  return ()

headingDiagram' :: Int -> XAxisConfig -> Heading -> MySvg
headingDiagram' i xConf@XAxisConfig{..} heading =
  mconcat (clockWindows ++ githubCommits ++ [background])

  where

    background    = timeRectMay xConf (headingTimes heading)
    clockWindows  = map (timeRectMay xConf . clockTimes) .
                    catMaybes . map fst .
                    sectionClocks . section $ heading
    githubCommits = []


fetchOrgFile :: (MonadWidget t m)
             => Event t a
             -> m (Event t (Either String Document))
fetchOrgFile trigger =
  let orgReq = XhrRequest "GET" myOrgUrl $
               def
      repMap = maybe (Left "No data")
               (T.parseOnly (parseDocument ["TODO","DONE"]))
  in do
    rep <- performRequestAsync (fmap (\_ -> orgReq) trigger)
    return $ fmap (repMap . _xhrResponse_body) rep

data GithubCommit = GithubCommit {
    ghCommitWho  :: T.Text
  , ghCommitWhen :: UTCTime
  , ghCommitText :: T.Text
  , ghCommitLink :: T.Text -- TODO make this a URL
  } deriving (Eq,Ord,Show)

orgWidget :: (MonadWidget t m) => Dynamic t (Document) -> m (Dynamic t A.Value)
orgWidget docDyn = do

  --docHeadings <- forDyn docDyn (\doc -> zip [0..] (documentHeadings doc))
  --(headingInfos) <- listViewWithKey docDyn $ \k h -> do
  --  orgHeadingWidget (constDyn xAxis0) k h   --  TODO: Update the axis on clicks
  --svgWidget $ constDyn (mconcat (zipWith (headingDiagram )[0..length docHeadings - 1] docHeadings))
  --svgWidget $ constDyn (timeLegend (xAxis0)) --  TODO: Update xaxis clicks
  dynSvg <- combineDyn orgSvg (constDyn xAxis0) docDyn
  svgWidget dynSvg
  return (constDyn $ A.Object $ HM.fromList [])

orgSvg :: XAxisConfig -> Document -> MySvg
orgSvg axis doc = g_ $ do
  mconcat (zipWith (headingDiagram axis) [0..length hs - 1] hs)
  timeLegend axis
    where hs = documentHeadings doc

orgHeadingWidget :: (MonadWidget t m)
                 => (Dynamic t XAxisConfig)
                 -> Int
                 -> Dynamic t Heading
                 -> m (Event t A.Value)
orgHeadingWidget xConfig k headingDyn = do
  dynHeadingSvg <- combineDyn (\c h -> headingDiagram c k h) xConfig headingDyn
  svgWidget dynHeadingSvg
  return (never)

svgWidget :: MonadWidget t m => Dynamic t MySvg -> m (El t)
svgWidget d = elDynHtml' "div" =<< mapDyn (TL.unpack . renderText . svg) d


svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [width_ "800", height_ "800", version_ "1.1"]

{-
diagramWidget :: MonadWidget t m => T.Text -> Dynamic t HeadingDiagram -> m (El t)
diagramWidget diaName d = elDynHtml' "div" =<< mapDyn h d
  where h = TL.unpack . renderText .
            renderDia SVG (SVGOptions spec Nothing diaName)
        spec = mkSizeSpec2D (Just 600 :: Maybe Double) (Just 50)
-}

------------------------------------------------------------------------------
timeLegend :: XAxisConfig -> MySvg
timeLegend xConf@XAxisConfig{..} = legendDia
  where
      isBetween t t0 t1         = t >= t0 && t <= t1

      toTimeParts :: UTCTime -> (Integer,Int,Int,Int,Int,Double)
      toTimeParts (UTCTime day t) =
        let (y,m,d)  = toGregorian day
            (_,w,wd) = toWeekDate  day
            h        = (realToFrac t / 3600)
        in  (y,m,w,d,wd,h)

      (thisY,thisM,thisW,thisD,thisWD,thisH) = toTimeParts tFocus
      (y0,m0,w0,d0,wd0,h0)                   = toTimeParts tStart
      (y1,m1,w1,d1,wd1,h1)                   = toTimeParts tEnd

      yearLabels  =
        [(utc,"%Y",3) | y   <- [y0..y1]
                      , let utc = UTCTime (fromGregorian y 0 0) 0
                      , isBetween utc tStart tEnd
                      ]

      monthLabels =
        [(utc,"%b",2) | m   <- [1..12]
                      , let utc = UTCTime (fromGregorian thisY m 0) 0
                      , isBetween utc tStart tEnd
                      ]

      wkDayLabels =
        [(utc,"%a",1) | wd  <- [1..7]
                      , let utc = UTCTime
                                (fromWeekDate thisY thisW wd) 0
                      ]

      toX   = tToX xConf

      toStr :: String -> UTCTime -> String
      toStr = formatTime defaultTimeLocale

      --baseLine = P (V2 (toX tStart) 0) ~~ P (V2 (toX tEnd) 0) # strokeP
      baseLine = line_ [x1_ (showF (toX tStart)), x2_ (showF (toX tEnd))
                       ,y1_ (showF 0), y2_ (showF 0)]

      --tickDia :: (UTCTime,String,Double) -> HeadingDiagram
      --tickDia (t,fmt,wght) =
      --  let p0 = P $ V2 0 0              :: Point V2 Double
      --      p1 = P $ V2 0 (-10 - 2*wght) :: Point V2 Double
      --      labelAndTick = (p0 ~~ p1 # strokeP)
      --                     ===
      --                     Diagrams.Prelude.text (toStr fmt t)
      --  in  labelAndTick # Diagrams.Prelude.translate (V2 (toX t) 0)

      tickDia (t,fmt,wght) = do
        line_ [x1_ (showF (toX t)), x2_ (showF (toX t))
              ,y1_ "0", y2_ (showF (-10 - 5*wght))]
        --text_ [x_ (showF (toX t)), y_ "20"] (T.pack $ toStr fmt t)

      legendDia = g_ $ do
        baseLine
        mconcat (map tickDia (yearLabels ++ monthLabels ++ wkDayLabels))
