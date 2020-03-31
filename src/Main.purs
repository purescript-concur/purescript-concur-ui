module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Element (Element)
import Element as E
import Element.Background as Background


main :: Effect Unit
main = runWidgetInDom "root" view

edges :: { bottom :: Int
, left :: Int
, right :: Int
, top :: Int
}
edges =
    { top: 0
    , right: 0
    , bottom: 0
    , left: 0
    }


view :: forall a. Widget HTML a
view =
  E.layout [ E.inFront header ] $
    E.column [ E.width E.fill ]
      [ header
      , hero
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
        ]


hero :: forall a. Element a
hero =
    E.row [ E.width E.fill, Background.color (E.rgb 0.0 0.7 0.0) ]
        [ E.image [ E.width E.fill ]
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
