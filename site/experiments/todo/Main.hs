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
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Clock
import           Reflex.Dom
import qualified Data.Text.Lazy       as TL
import           Diagrams.Prelude     hiding (Dynamic, section, el)
import           Diagrams.Backend.SVG
import           Lucid.Svg            hiding ((<>))

import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types

import Axis



myOrgUrl :: String
--myOrgUrl = "file:///home/greghale/Programming/CBMM.github.io/site/experiments/todo.org"
--myOrgUrl = "https://raw.githubusercontent.com/CBMM/CBMM.github.io/master/site/experiments/todo.org"
myOrgUrl = "https://cdn.rawgit.com/CBMM/CBMM.github.io/master/site/experiments/todo.org"


headingTimes :: Heading -> (Maybe UTCTime, Maybe UTCTime)
headingTimes heading =
  case sectionPlannings . section $ heading of
   Plns ps -> ((dateTimeToUTC . tsTime) <$> HM.lookup SCHEDULED ps,
               (dateTimeToUTC . tsTime) <$> HM.lookup CLOSED ps)

clockTimes :: Timestamp -> (Maybe UTCTime, Maybe UTCTime)
clockTimes (Timestamp t0 _ mT1) = (dateTimeToUTC <$> Just t0,
                                   dateTimeToUTC <$> mT1)

main :: IO ()
main = mainWidget $ mdo

  diagramWidget "head circle" (circle 100)
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

headingDiagram :: Int -> XAxisConfig -> Heading -> HeadingDiagram
headingDiagram i xConf@XAxisConfig{..} heading =
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
                 -> m (Event t A.Value)
--                 -> m (Event t T.Text)
orgHeadingWidget xConfig k headingDyn = do
  dynSvgs <- combineDyn (headingDiagram k) xConfig headingDyn
  diagramWidget "test" dynSvgs
  return never

diagramWidget :: MonadWidget t m => T.Text -> Dynamic t HeadingDiagram -> m ()
diagramWidget diaName d = elDynHtml' "div" (constDyn h) >> return ()
  where h = TL.unpack . renderText $
            renderDia SVG (SVGOptions spec Nothing diaName) d
        spec = mkSizeSpec2D (Just 300 :: Maybe Double) (Just 50)



{- Leftover code from orgHeadingWidget
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
-}
