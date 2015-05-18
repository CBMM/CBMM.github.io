{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Error (note)
import qualified Data.Attoparsec.Text as T
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Clock
import           Reflex.Dom

import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types

myOrgUrl :: String
--myOrgUrl = "file:///home/greghale/Programming/CBMM.github.io/site/experiments/todo.org"
--myOrgUrl = "https://raw.githubusercontent.com/CBMM/CBMM.github.io/master/site/experiments/todo.org"
myOrgUrl = "https://cdn.rawgit.com/CBMM/CBMM.github.io/master/site/experiments/todo.org"

main :: IO ()
main = mainWidget $ mdo

  circleWidget
  refreshClick <- button "Refresh org data"
  fetchOrgTriggers <- appendEvents refreshClick <$> getPostBuild
  -- TODO: Data doesn't change on reload after changing todo.org. why?
  orgEvents <- flip fforMaybe (either (const Nothing) Just) <$> fetchOrgFile fetchOrgTriggers
  orgData <- holdDyn (Document "No document yet" [])
             orgEvents
  el "div" $ orgWidget orgData
  --dynText =<< mapDyn show orgData
  return ()

fetchOrgFile :: (MonadWidget t m)
             => Event t a
             -> m (Event t (Either String Document))
fetchOrgFile trigger =
  let orgReq = XhrRequest "GET" myOrgUrl $
               def --{_xhrRequestConfig_headers =
                   -- Map.fromList [("Origin","raw.githubusercontent.com")]}
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

  docDyn       <- forDyn docDyn (\doc -> Map.fromList $ zip [0..] (documentHeadings doc))
  elAttr "svg" (Map.fromList [("width","300"),("height","300"),("style","backgrond-color:green")]) $ do
    headingInfos <- listViewWithKey docDyn $ \k h -> do
      orgHeadingWidget (constDyn xAxis0) k h
    return (constDyn $ A.Object $ HM.fromList [])

orgHeadingWidget :: (MonadWidget t m)
                 => (Dynamic t XAxisConfig)
                 -> Int
                 -> Dynamic t Heading
                 -> m (Event t T.Text)
orgHeadingWidget xConfig k heading = do

  boxAttrsDyn <- combineDyn attrFun xConfig heading
  elDynAttr "rect" boxAttrsDyn $
    dynText =<< mapDyn (T.unpack . title) heading
  return $ never

  where
    height = 20 :: Int
    xAtPln :: XAxisConfig -> PlanningKeyword -> UTCTime -> Heading -> Int
    xAtPln xConf plName defaultT h =
      case sectionPlannings (section h) of
       Plns hm -> case HM.lookup plName hm of
         Nothing -> tToX xConf defaultT
         Just (Timestamp ts _ _) -> tToX xConf (dateTimeToUTC ts)

    attrFun :: XAxisConfig -> Heading -> Map.Map String String
    attrFun xConf h =
      let x0 = xAtPln xConf SCHEDULED (tStart xConf) h
          x1 = xAtPln xConf CLOSED (tEnd xConf) h
      in  Map.fromList [("x", show x0)
                       ,("width", show (x1-x0))
                       ,("height", show height)
                       ,("y", show (height*k))
                       ]

dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC (DateTime (YMD' (YearMonthDay y m d)) _ mayHourMin _ _) =
  let theHour = maybe 0 fst mayHourMin
      theMin  = maybe 0 snd mayHourMin
      secsIn  = 60*60*theHour + 60*theMin
  in  UTCTime (fromGregorian (fromIntegral y) m d) (fromIntegral secsIn)

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
  tStart = UTCTime (fromGregorian 2014 1 1) 0
  , tEnd = UTCTime (fromGregorian 2017 1 1) 0
  , xStart = 10
  , xEnd   = 210
  , tFocus  = UTCTime (fromGregorian 2016 5 20) 0
  , zFrac = 0.5
  }

tToX :: XAxisConfig -> UTCTime -> Int
tToX XAxisConfig{..} t =
  let r2 = realToFrac :: Real x => x -> Double
      m = (xEnd - xStart) /
          realToFrac (diffUTCTime tEnd tStart)
      xNoZoom   = (r2 $ diffUTCTime t tStart) * m + xStart
      xFullZoom
        | t == tFocus = xNoZoom
        | t >  tFocus = xEnd
        | otherwise   = xStart
      x = xNoZoom * (1-zFrac) + xFullZoom * zFrac
  in floor x

circleWidget :: (MonadWidget t m) => m ()
circleWidget = elAttr "svg" (Map.fromList [("width","200"),("height","100")]) $
               elAttr "circle" (Map.fromList [("cx","50")
                                             ,("cy","50")
                                             ,("r","40")
                                             ,("stroke","green")
                                             ,("stroke-width","4")
                                             ,("fill","yellow")]) (return ())
