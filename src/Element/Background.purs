module Element.Background
    ( color, gradient
    , image, uncropped, tiled, tiledX, tiledY
    ) where

{-|

@docs color, gradient


# Images

@docs image, uncropped, tiled, tiledX, tiledY

**Note** if you want more control over a background image than is provided here, you should try just using a normal `Element.image` with something like `Element.behindContent`.

-}

import Concur.React.Props as P
import Data.Array ((:))
import Data.Array as Array
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as String
import Element (Attr, Attribute, Color)
import Internal.Flag as Flag
import Internal.Model as Internal


{-| -}
color :: forall decorative msg. Color -> Attr decorative msg
color clr =
    Internal.StyleClass Flag.bgColor (Internal.Colored ("bg-" <> Internal.formatColorClass clr) "background-color" clr)


{-| Resize the image to fit the containing element while maintaining proportions and cropping the overflow.
-}
image :: forall msg. String -> Attribute msg
image src =
    Internal.Attr (P.style {background: "url(\"" <> src <> "\") center / cover no-repeat"})


{-| A centered background image that keeps its natural proportions, but scales to fit the space.
-}
uncropped :: forall msg. String -> Attribute msg
uncropped src =
    Internal.Attr (P.style {background: "url(\"" <> src <> "\") center / contain no-repeat"})


{-| Tile an image in the x and y axes.
-}
tiled :: forall msg. String -> Attribute msg
tiled src =
    Internal.Attr (P.style {background: "url(\"" <> src <> "\") repeat"})


{-| Tile an image in the x axis.
-}
tiledX :: forall msg. String -> Attribute msg
tiledX src =
    Internal.Attr (P.style {background: "url(\"" <> src <> "\") repeat-x"})


{-| Tile an image in the y axis.
-}
tiledY :: forall msg. String -> Attribute msg
tiledY src =
    Internal.Attr (P.style {background: "url(\"" <> src <> "\") repeat-y"})


data Direction
    = ToUp
    | ToDown
    | ToRight
    | ToTopRight
    | ToBottomRight
    | ToLeft
    | ToTopLeft
    | ToBottomLeft
    | ToAngle Number


data Step
    = ColorStep Color
    | PercentStep Number Color
    | PxStep Int Color


{-| -}
step :: Color -> Step
step =
    ColorStep


{-| -}
percent :: Number -> Color -> Step
percent =
    PercentStep


{-| -}
px :: Int -> Color -> Step
px =
    PxStep


{-| A linear gradient.

First you need to specify what direction the gradient is going by providing an angle in radians. `0` is up and `pi` is down.

The colors will be evenly spaced.

-}
gradient :: forall decorative msg.
    { angle :: Number
    , steps :: Array Color
    }
    -> Attr decorative msg
gradient { angle, steps } =
    case Array.uncons steps of
        Nothing ->
            Internal.NoAttribute

        Just {head:clr, tail} ->
          case Array.uncons tail of
            Nothing ->
              Internal.StyleClass Flag.bgColor $
                  Internal.Colored ("bg-" <> Internal.formatColorClass clr) "background-color" clr
            _ ->
                Internal.StyleClass Flag.bgGradient $
                    Internal.Single ("bg-grad-" <> (String.joinWith "-" $ Internal.floatClass angle : map Internal.formatColorClass steps))
                    "background-image"
                    ("linear-gradient(" <> (String.joinWith ", " $ (show angle <> "rad") : map Internal.formatColor steps) <> ")") -- Number



-- {-| -}
-- gradientWith :: { direction :: Direction, steps :: Array Step } -> Attribute msg
-- gradientWith { direction, steps } =
--     StyleClass #
--         Single ("bg-gradient-" <> (String.joinWith "-" # renderDirectionClass direction :: map renderStepClass steps))
--             "background"
--             ("linear-gradient(" <> (String.joinWith ", " # renderDirection direction :: map renderStep steps) <> ")")
-- {-| -}
-- renderStep :: Step -> String
-- renderStep step =
--     case step of
--         ColorStep color ->
--             formatColor color
--         PercentStep percent color ->
--             formatColor color <> " " <> toString percent <> "%"
--         PxStep px color ->
--             formatColor color <> " " <> toString px <> "px"
-- {-| -}
-- renderStepClass :: Step -> String
-- renderStepClass step =
--     case step of
--         ColorStep color ->
--             formatColorClass color
--         PercentStep percent color ->
--             formatColorClass color <> "-" <> floatClass percent <> "p"
--         PxStep px color ->
--             formatColorClass color <> "-" <> toString px <> "px"
-- toUp :: Direction
-- toUp =
--     ToUp
-- toDown :: Direction
-- toDown =
--     ToDown
-- toRight :: Direction
-- toRight =
--     ToRight
-- toTopRight :: Direction
-- toTopRight =
--     ToTopRight
-- toBottomRight :: Direction
-- toBottomRight =
--     ToBottomRight
-- toLeft :: Direction
-- toLeft =
--     ToLeft
-- toTopLeft :: Direction
-- toTopLeft =
--     ToTopLeft
-- toBottomLeft :: Direction
-- toBottomLeft =
--     ToBottomLeft
-- angle :: Number -> Direction
-- angle rad =
--     ToAngle rad
-- renderDirection :: Direction -> String
-- renderDirection dir =
--     case dir of
--         ToUp ->
--             "to top"
--         ToDown ->
--             "to bottom"
--         ToRight ->
--             "to right"
--         ToTopRight ->
--             "to top right"
--         ToBottomRight ->
--             "to bottom right"
--         ToLeft ->
--             "to left"
--         ToTopLeft ->
--             "to top left"
--         ToBottomLeft ->
--             "to bottom left"
--         ToAngle angle ->
--             toString angle <> "rad"
-- renderDirectionClass :: Direction -> String
-- renderDirectionClass dir =
--     case dir of
--         ToUp ->
--             "to-top"
--         ToDown ->
--             "to-bottom"
--         ToRight ->
--             "to-right"
--         ToTopRight ->
--             "to-top-right"
--         ToBottomRight ->
--             "to-bottom-right"
--         ToLeft ->
--             "to-left"
--         ToTopLeft ->
--             "to-top-left"
--         ToBottomLeft ->
--             "to-bottom-left"
--         ToAngle angle ->
--             floatClass angle <> "rad"
