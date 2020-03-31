module Element.Border
    ( color
    , width, widthXY, widthEach
    , solid, dashed, dotted
    , rounded, roundEach
    , glow, innerGlow, shadow, innerShadow
    ) where

{-|

@docs color


## Border Widths

@docs width, widthXY, widthEach


## Border Styles

@docs solid, dashed, dotted


## Rounded Corners

@docs rounded, roundEach


## Shadows

@docs glow, innerGlow, shadow, innerShadow

-}

import Data.BooleanAlgebra ((&&))
import Data.CommutativeRing ((*))
import Data.Eq ((==))
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Element (Attr, Attribute, Color)
import Internal.Flag as Flag
import Internal.Model (Shadow)
import Internal.Model as Internal
import Internal.Style (classes) as IStyle


{-| -}
color :: forall decorative msg. Color -> Attr decorative msg
color clr =
    Internal.StyleClass
        Flag.borderColor
        (Internal.Colored
            ("bc-" <> Internal.formatColorClass clr)
            "border-color"
            clr
        )


{-| -}
width :: forall msg. Int -> Attribute msg
width v =
    Internal.StyleClass
        Flag.borderWidth
        (Internal.BorderWidth
            ("b-" <> show v) -- Int
            v
            v
            v
            v
        )


{-| Set horizontal and vertical borders.
-}
widthXY :: forall msg. Int -> Int -> Attribute msg
widthXY x y =
    Internal.StyleClass
        Flag.borderWidth
        (Internal.BorderWidth
            ("b-"
                <> show x -- Int
                <> "-"
                <> show y -- Int
            )
            y
            x
            y
            x
        )


{-| -}
widthEach :: forall msg.
    { bottom :: Int
    , left :: Int
    , right :: Int
    , top :: Int
    }
    -> Attribute msg
widthEach { bottom, top, left, right } =
    if top == bottom && left == right then
        if top == right then
            width top

        else
            widthXY left top

    else
        Internal.StyleClass Flag.borderWidth
            (Internal.BorderWidth
                ("b-"
                    <> show top -- Int
                    <> "-"
                    <> show right -- Int
                    <> "-"
                    <> show bottom -- Int
                    <> "-"
                    <> show left -- Int
                )
                top
                right
                bottom
                left
            )



-- {-| No Borders
-- -}
-- none :: Attribute msg
-- none =
--     Class "border" "border-none"


{-| -}
solid :: forall msg. Attribute msg
solid =
    Internal.Class Flag.borderStyle IStyle.classes.borderSolid


{-| -}
dashed :: forall msg. Attribute msg
dashed =
    Internal.Class Flag.borderStyle IStyle.classes.borderDashed


{-| -}
dotted :: forall msg. Attribute msg
dotted =
    Internal.Class Flag.borderStyle IStyle.classes.borderDotted


{-| Round all corners.
-}
rounded :: forall msg. Int -> Attribute msg
rounded radius =
    Internal.StyleClass
        Flag.borderRound
        (Internal.Single
            ("br-" <> show radius) -- Int
            "border-radius"
            (show radius <> "px") -- Int
        )


{-| -}
roundEach :: forall msg.
    { topLeft :: Int
    , topRight :: Int
    , bottomLeft :: Int
    , bottomRight :: Int
    }
    -> Attribute msg
roundEach { topLeft, topRight, bottomLeft, bottomRight } =
    Internal.StyleClass Flag.borderRound
        (Internal.Single
            ("br-"
                <> show topLeft -- Int
                <> "-"
                <> show topRight -- Int
                <> show bottomLeft -- Int
                <> "-"
                <> show bottomRight -- Int
            )
            "border-radius"
            (show topLeft -- Int
                <> "px "
                <> show topRight -- Int
                <> "px "
                <> show bottomRight -- Int
                <> "px "
                <> show bottomLeft -- Int
                <> "px"
            )
        )


{-| A simple glow by specifying the color and size.
-}
glow :: forall decorative msg. Color -> Number -> Attr decorative msg
glow clr size =
    shadow
        { offset: { xval: 0.0, yval:0.0 }
        , size: size
        , blur: size * 2.0
        , color: clr
        }


{-| -}
innerGlow :: forall decorative msg. Color -> Number -> Attr decorative msg
innerGlow clr size =
    innerShadow
        { offset: { xval:0.0, yval:0.0 }
        , size: size
        , blur: size * 2.0
        , color: clr
        }


{-| -}
shadow :: forall decorative msg. Shadow -> Attr decorative msg
shadow almostShade =
    let
        shade =
            { offset: almostShade.offset
            , size: almostShade.size
            , blur: almostShade.blur
            , color: almostShade.color
            }
    in
    Internal.StyleClass Flag.shadows $
        Internal.Single
            (Internal.boxShadowClass false shade)
            "box-shadow"
            (Internal.formatBoxShadow false shade)


{-| -}
innerShadow :: forall decorative msg. Shadow -> Attr decorative msg
innerShadow almostShade =
    let
        shade =
            { offset: almostShade.offset
            , size: almostShade.size
            , blur: almostShade.blur
            , color: almostShade.color
            }
    in
    Internal.StyleClass Flag.shadows $
        Internal.Single
            (Internal.boxShadowClass true shade)
            "box-shadow"
            (Internal.formatBoxShadow true shade)



-- {-| -}
-- shadow ::
--     { offset :: ( Number, Number )
--     , blur :: Number
--     , size :: Number
--     , color :: Color
--     }
--     -> Attr decorative msg
-- shadow shade =
--     Internal.BoxShadow
--         { inset = False
--         , offset = shade.offset
--         , size = shade.size
--         , blur = shade.blur
--         , color = shade.color
--         }
