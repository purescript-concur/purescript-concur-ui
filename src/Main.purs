module Main where

import Prelude

import Concur.Core (Widget)
import Concur.Core.LiftWidget (liftWidget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Either (Either(..))
import Data.Int as I
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Element as E
import Element as Element

drawUI :: forall a. Int -> Int -> Widget HTML a
drawUI padding spacing = do
  res <- center padding spacing $ D.div'
    [ sampleWidget
    , D.div'
        [ Left <$> mySlider padding
        , Right <$> mySlider spacing
        ]
    ]
  case res of
    Left (Just padding') -> drawUI padding' spacing
    Right (Just spacing') -> drawUI padding spacing'
    _ -> drawUI padding spacing
  where
    mySlider = slider 0 100 1

-- My sample widget
sampleWidget :: forall a. Widget HTML a
sampleWidget = do
  void $ D.button [P.onClick] [D.text "Say Hello"]
  D.text "Hello Sailor!"

-- HTML5 Slider
slider :: Int -> Int -> Int -> Int -> Widget HTML (Maybe Int)
slider min max steps val = do
  D.input
    [ P._type "range"
    , P.defaultValue (show val)
    , P.min (show min)
    , P.max (show max)
    , I.fromString <<< P.unsafeTargetValue <$> P.onInput
    , P.unsafeMkProp "step" steps
    ]

main :: Effect Unit
main = runWidgetInDom "root" (drawUI 20 20)

-- Elm-UI layout!

-- Center a widget with a custom padding and spacing
center :: forall a. Int -> Int -> Widget HTML a -> Widget HTML a
center padding spacing w =
  Element.layout [] $
    E.el [ E.centerX, E.centerY, E.padding padding, E.spacing spacing ] (liftWidget w)
