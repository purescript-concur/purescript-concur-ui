module Main where

import Prelude

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Int (toNumber)
import Effect (Effect)
import Element (Element)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Internal.Model as X
import Util (zeroDiv)


main :: Effect Unit
main = runWidgetInDom "root" (E.layout [] view)

type Layout =
  { bottom :: Int
  , left :: Int
  , right :: Int
  , top :: Int
  }

edges :: Layout
edges =
    { top: 0
    , right: 0
    , bottom: 0
    , left: 0
    }


view :: forall a. Element a
view =
    E.el [ E.inFront header ] $
      E.column [ E.width E.fill ]
        [ hero
        , content
        , footer
        ]

header :: forall a. Element a
header =
    E.row [ E.width E.fill, Background.color (E.rgb 0.0 0.5 0.0), E.spacing 20, E.padding 30 ]
        [ E.el [] (E.text "logo")
        , E.el [ E.alignRight ] (E.text "Home")
        , E.el [] (E.text "About Us")
        , E.el [] (E.text "Services")
        , E.el [] (E.text "Contact Us")
        , E.el [] (X.unstyled myCheckbox)
        ]


hero :: forall a. Element a
hero =
    E.row [ E.width E.fill, Background.color (E.rgb 0.0 0.7 0.0) ]
        [ E.none
        , E.image [ E.width E.fill ]
            { src: "https://via.placeholder.com/1400x400"
            , description: "Hero Image"
            }
        ]


content :: forall a.Element a
content =
    E.column
        [ E.width
            (E.fill
                # E.maximum 750
                # E.minimum 250
            )
        , Background.color (E.rgb 0.0 0.9 0.0)
        , E.centerX
        , E.spacing 10
        , E.padding 10
        ]
        [ E.textColumn []
            [ E.image [ E.alignRight ]
                { src: "https://via.placeholder.com/250"
                , description: "Hero Image"
                }
            , home
            ]
        , E.textColumn []
            [ E.image [ E.paddingEach edges{ right = 10 }, E.alignLeft ]
                { src: "https://via.placeholder.com/250"
                , description: "Hero Image"
                }
            , about
            ]
        , E.textColumn []
            [ E.image [ E.alignRight ]
                { src: "https://via.placeholder.com/250"
                , description: "Hero Image"
                }
            , services
            ]
        , E.textColumn []
            [ E.image [ E.paddingEach edges{ right = 10 }, E.alignLeft ]
                { src: "https://via.placeholder.com/250"
                , description: "Hero Image"
                }
            , contactUs
            ]
        ]


home :: forall a. Element a
home =
    E.paragraph []
        [ E.text
            "Home lots of text ...."
        ]


about :: forall a. Element a
about =
    E.paragraph []
        [ E.text
            "About lots of text ...."
        ]


services :: forall a. Element a
services =
    E.paragraph []
        [ E.text
            "lots of text ...."
        ]


contactUs :: forall a. Element a
contactUs =
    E.paragraph []
        [ E.text
            "lots of text ...."
        ]


footer :: forall a. Element a
footer =
    E.row [ E.height (E.px 200), E.width E.fill, Background.color (E.rgb 0.0 0.3 0.0) ] []

toggleCheckboxWidget :: { offColor :: E.Color, onColor :: E.Color, sliderColor :: E.Color, toggleWidth :: Int, toggleHeight :: Int } -> Boolean -> Element Unit
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleWidth, toggleHeight } checked =
  E.el
    [ Background.color $ if checked then onColor else offColor
    , E.width $ E.px toggleWidth
    , E.height $ E.px toggleHeight
    , Border.rounded 14
    , unit <$ E.htmlAttribute (P.onClick)
    , E.inFront $
        E.el [ E.height E.fill ] $
            E.el
                [ Background.color sliderColor
                , Border.rounded $ sliderSize `zeroDiv` 2
                , E.width $ E.px sliderSize
                , E.height $ E.px sliderSize
                , E.centerY
                , E.moveRight (toNumber pad)
                , E.htmlAttribute $ P.style
                    { transition: ".4s"
                    , transform: if checked then "translateX(" <> translation <> "px)" else ""
                    }
                ]
                (E.text "")
    ]
    (E.text "")
  where
    pad = 3
    sliderSize = toggleHeight - 2 * pad
    translation = show (toggleWidth - sliderSize - pad)

myCheckbox :: forall a. Widget HTML a
myCheckbox = go false
  where
    go b = do
      myCheckboxView b
      go (not b)

myCheckboxView :: Boolean -> Widget HTML Unit
myCheckboxView b = E.layoutInner [] $
  toggleCheckboxWidget opts b
  where
    opts =
      { offColor: lightGrey
      , onColor: green
      , sliderColor: white
      , toggleWidth: 60
      , toggleHeight: 28
      }
    lightGrey = E.rgb255 187 187 187
    green = E.rgb255 39 203 139
    white = E.rgb255 255 255 255
