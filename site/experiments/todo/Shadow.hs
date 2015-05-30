{-# LANGUAGE OverloadedStrings #-}

module Shadow where

import Data.Foldable
import qualified Data.Text as T
import Lucid.Svg
import qualified Lucid.Svg.Attributes as A
import qualified Lucid.Svg.Elements   as E


shadowDefs :: Int -> Int -> Int -> T.Text -> Svg ()
shadowDefs x y blur filtId = defs_ $ do
  (term "filter") fParams $ do
    feOffset_       [result_ "offOut", in_ "SourceAlpha"
                    , dx_ (T.pack $ show x), dy_ (T.pack $ show y)]
    feGaussianBlur_ [result_ "blurOut", in_ "offOut"
                    , stdDeviation_ (T.pack $ show blur)]
    feBlend_        [in_ "SourceGraphic", in2_ "blurOut"
                    , mode_ "normal"]
  where
    fParams = [ id_ filtId , x_ "-2" , y_ "-2"
              , width_ "1000%" , height_ "1000%"]

svg :: Svg () -> Svg ()
svg content = do
  with (svg11_ (content))
    [version_ "1.1", width_ "700", height_ "500"]

dropShadow x y blur el = do
  shadowDefs x y blur filtName
  with el [A.filter_ filtUrl]
  where filtName = T.pack $ "shadowFiltX" ++ show x ++ "Y" ++ show y
                            ++ "B" ++ show blur
        filtUrl = T.concat ["url(#", filtName, ")"]
