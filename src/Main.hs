{-# LANGUAGE TemplateHaskell #-}

module Main where

import Universum
import Flow

import LitoUtils
import qualified Pos
import qualified Lib
import Debug

import Control.Lens hiding ((^.), (|>), (<|))
import Data.Text (Text)
import Monomer
import TextShow

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Eq, Show)

makeLenses 'AppModel

menuBarStyle :: CmbStyleBasic t => [t] -> [t]
menuBarStyle = map (`styleBasic` [
  radius 0
  -- bgColor darkGray,
  -- hlColor  lightGray,
  -- sndColor lightGray
  ])

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      hstack <| menuBarStyle [
        button "File" AppInit,
        button "Edit" AppInit,
        button "View" AppInit,
        button "Move" AppInit
      ],
      separatorLine,
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease
      ]
    ]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "R",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0
