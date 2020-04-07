module Element.Font
    ( color, size
    , family, Font, typeface, serif, sansSerif, monospace
    , external
    , alignLeft, alignRight, center, justify, letterSpacing, wordSpacing
    , underline, strike, italic, unitalicized
    , heavy, extraBold, bold, semiBold, medium, regular, light, extraLight, hairline
    , Variant, variant, variantArray, smallCaps, slashedZero, ligatures, ordinal, tabularNumbers, stackedFractions, diagonalFractions, swash, feature, indexed
    , glow, shadow
    ) where

{-|
    import Element
    import Element.Font as Font
    view =
        Element.el
            [ Font.color (Element.rgb 0 0 1)
            , Font.size 18
            , Font.family
                [ Font.typeface "Open Sans"
                , Font.sansSerif
                ]
            ]
            (Element.text "Woohoo, I'm stylish text")
**Note:** `Font.color`, `Font.size`, and `Font.family` are inherited, meaning you can set them at the top of your view and all subsequent nodes will have that value.
**Other Note:** If you're looking for something like `line-height`, it's handled by `Element.spacing` on a `paragraph`.
@docs color, size
## Typefaces
@docs family, Font, typeface, serif, sansSerif, monospace
@docs external
## Alignment and Spacing
@docs alignLeft, alignRight, center, justify, letterSpacing, wordSpacing
## Font Styles
@docs underline, strike, italic, unitalicized
## Font Weight
@docs heavy, extraBold, bold, semiBold, medium, regular, light, extraLight, hairline
## Variants
@docs Variant, variant, variantArray, smallCaps, slashedZero, ligatures, ordinal, tabularNumbers, stackedFractions, diagonalFractions, swash, feature, indexed
## Shadows
@docs glow, shadow
-}

import Data.Array as Array
import Data.CommutativeRing ((*))
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as String
import Element (Attr, Attribute, Color)
import Internal.Flag as Flag
import Internal.Model (XYVal, arrayFoldl)
import Internal.Model as Internal
import Internal.Style (classes)


{-| -}
type Font =
    Internal.Font


{-| -}
color :: forall decorative msg. Color -> Attr decorative msg
color fontColor =
    Internal.StyleClass
        Flag.fontColor
        (Internal.Colored
            ("fc-" <> Internal.formatColorClass fontColor)
            "color"
            fontColor
        )


{-|
    import Element
    import Element.Font as Font
    myElement =
        Element.el
            [ Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            ]
            (text "")
-}
family :: forall msg. Array Font -> Attribute msg
family families =
    Internal.StyleClass
        Flag.fontFamily
        (Internal.FontFamily
            (arrayFoldl Internal.renderFontClassName "ff-" families)
            families
        )


{-| -}
serif :: Font
serif =
    Internal.Serif


{-| -}
sansSerif :: Font
sansSerif =
    Internal.SansSerif


{-| -}
monospace :: Font
monospace =
    Internal.Monospace


{-| -}
typeface :: String -> Font
typeface =
    Internal.Typeface


{-| -}
type Adjustment =
    { capital :: Number
    , lowercase :: Number
    , baseline :: Number
    , descender :: Number
    }


{-| -}
with ::
    { name :: String
    , adjustment :: Maybe Adjustment
    , variants :: Array Variant
    }
    -> Font
with =
    Internal.FontWith


{-| -}
sizeByCapital :: forall msg. Attribute msg
sizeByCapital =
    Internal.htmlClass classes.sizeByCapital


{-| -}
full :: forall msg. Attribute msg
full =
    Internal.htmlClass classes.fullSize


{-| **Note** it's likely that `Font.external` will cause a flash on your page on loading.
To bypass this, import your fonts using a separate stylesheet and just use `Font.typeface`.
It's likely that `Font.external` will be removed or redesigned in the future to avoid the flashing.
`Font.external` can be used to import font files. Let's say you found a neat font on <http://fonts.google.com>:
    import Element
    import Element.Font as Font
    view =
        Element.el
            [ Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]
            ]
            (Element.text "Woohoo, I'm stylish text")
-}
external :: { url :: String, name :: String } -> Font
external { url, name } =
    Internal.ImportFont name url


{-| Font sizes are always given as `px`.
-}
size :: forall decorative msg. Int -> Attr decorative msg
size i =
    Internal.StyleClass Flag.fontSize (Internal.FontSize i)


{-| In `px`.
-}
letterSpacing :: forall msg. Number -> Attribute msg
letterSpacing offset =
    Internal.StyleClass Flag.letterSpacing $
        Internal.Single
            ("ls-" <> Internal.floatClass offset)
            "letter-spacing"
            (show offset <> "px") -- Float


{-| In `px`.
-}
wordSpacing :: forall msg. Number -> Attribute msg
wordSpacing offset =
    Internal.StyleClass Flag.wordSpacing $
        Internal.Single ("ws-" <> Internal.floatClass offset) "word-spacing" (show offset <> "px") -- Float


{-| Align the font to the left.
-}
alignLeft :: forall msg. Attribute msg
alignLeft =
    Internal.Class Flag.fontAlignment classes.textLeft


{-| Align the font to the right.
-}
alignRight :: forall msg. Attribute msg
alignRight =
    Internal.Class Flag.fontAlignment classes.textRight


{-| Center align the font.
-}
center :: forall msg. Attribute msg
center =
    Internal.Class Flag.fontAlignment classes.textCenter


{-| -}
justify :: forall msg. Attribute msg
justify =
    Internal.Class Flag.fontAlignment classes.textJustify



-- {-| -}
-- justifyAll :: Attribute msg
-- justifyAll =
--     Internal.class classesTextJustifyAll


{-| -}
underline :: forall msg. Attribute msg
underline =
    Internal.htmlClass classes.underline


{-| -}
strike :: forall msg. Attribute msg
strike =
    Internal.htmlClass classes.strike


{-| -}
italic :: forall msg. Attribute msg
italic =
    Internal.htmlClass classes.italic


{-| -}
bold :: forall msg. Attribute msg
bold =
    Internal.Class Flag.fontWeight classes.bold


{-| -}
light :: forall msg. Attribute msg
light =
    Internal.Class Flag.fontWeight classes.textLight


{-| -}
hairline :: forall msg. Attribute msg
hairline =
    Internal.Class Flag.fontWeight classes.textThin


{-| -}
extraLight :: forall msg. Attribute msg
extraLight =
    Internal.Class Flag.fontWeight classes.textExtraLight


{-| -}
regular :: forall msg. Attribute msg
regular =
    Internal.Class Flag.fontWeight classes.textNormalWeight


{-| -}
semiBold :: forall msg. Attribute msg
semiBold =
    Internal.Class Flag.fontWeight classes.textSemiBold


{-| -}
medium :: forall msg. Attribute msg
medium =
    Internal.Class Flag.fontWeight classes.textMedium


{-| -}
extraBold :: forall msg. Attribute msg
extraBold =
    Internal.Class Flag.fontWeight classes.textExtraBold


{-| -}
heavy :: forall msg. Attribute msg
heavy =
    Internal.Class Flag.fontWeight classes.textHeavy


{-| This will reset bold and italic.
-}
unitalicized :: forall msg. Attribute msg
unitalicized =
    Internal.htmlClass classes.textUnitalicized


{-| -}
shadow :: forall decorative msg.
    { offset :: XYVal Number
    , blur :: Number
    , color :: Color
    }
    -> Attr decorative msg
shadow shade =
    Internal.StyleClass Flag.txtShadows $
        Internal.Single (Internal.textShadowClass shade) "text-shadow" (Internal.formatTextShadow shade)


{-| A glow is just a simplified shadow.
-}
glow :: forall decorative msg. Color -> Number -> Attr decorative msg
glow clr i =
    let
        shade =
            { offset: { xval:0.0, yval:0.0 }
            , blur: i * 2.0
            , color: clr
            }
    in
    Internal.StyleClass Flag.txtShadows $
        Internal.Single (Internal.textShadowClass shade) "text-shadow" (Internal.formatTextShadow shade)



{- Variants -}


{-| -}
type Variant =
    Internal.Variant


{-| You can use this to set a single variant on an element itself such as:
    el
        [ Font.variant Font.smallCaps
        ]
        (text "rendered with smallCaps")
**Note** These will **not** stack. If you want multiple variants, you should use `Font.variantArray`.
-}
variant :: forall msg. Variant -> Attribute msg
variant var =
    case var of
        Internal.VariantActive name ->
            Internal.Class Flag.fontVariant ("v-" <> name)

        Internal.VariantOff name ->
            Internal.Class Flag.fontVariant ("v-" <> name <> "-off")

        Internal.VariantIndexed name index ->
            Internal.StyleClass Flag.fontVariant $
                Internal.Single ("v-" <> name <> "-" <> show index) -- Int
                    "font-feature-settings"
                    ("\"" <> name <> "\" " <> show index) -- Int


isSmallCaps :: Variant -> Boolean
isSmallCaps x =
    case x of
        Internal.VariantActive feat ->
            feat == "smcp"

        _ ->
            false


{-| -}
variantArray :: forall msg. Array Variant -> Attribute msg
variantArray vars =
    let
        features =
            vars
                # map Internal.renderVariant

        hasSmallCaps =
            Array.any isSmallCaps vars

        name =
            if hasSmallCaps then
                vars
                    # map Internal.variantName
                    # String.joinWith "-"
                    # (\x -> x <> "-sc")

            else
                vars
                    # map Internal.variantName
                    # String.joinWith "-"

        featureString =
            String.joinWith ", " features
    in
    Internal.StyleClass Flag.fontVariant $
        Internal.Style ("v-" <> name)
            [ Internal.Property "font-feature-settings" featureString
            , Internal.Property "font-variant"
                (if hasSmallCaps then
                    "small-caps"

                 else
                    "normal"
                )
            ]


{-| [Small caps](https://en.wikipedia.org/wiki/Small_caps) are rendered using uppercase glyphs, but at the size of lowercase glyphs.
-}
smallCaps :: Variant
smallCaps =
    Internal.VariantActive "smcp"


{-| Add a slash when rendering `0`
-}
slashedZero :: Variant
slashedZero =
    Internal.VariantActive "zero"


{-| -}
ligatures :: Variant
ligatures =
    Internal.VariantActive "liga"


{-| Oridinal markers like `1st` and `2nd` will receive special glyphs.
-}
ordinal :: Variant
ordinal =
    Internal.VariantActive "ordn"


{-| Number figures will each take up the same space, allowing them to be easily aligned, such as in tables.
-}
tabularNumbers :: Variant
tabularNumbers =
    Internal.VariantActive "tnum"


{-| Render fractions with the numerator stacked on top of the denominator.
-}
stackedFractions :: Variant
stackedFractions =
    Internal.VariantActive "afrc"


{-| Render fractions
-}
diagonalFractions :: Variant
diagonalFractions =
    Internal.VariantActive "frac"


{-| -}
swash :: Int -> Variant
swash =
    Internal.VariantIndexed "swsh"


{-| Set a feature by name and whether it should be on or off.
Feature names are four-letter names as defined in the [OpenType specification](https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist).
-}
feature :: String -> Boolean -> Variant
feature name on =
    if on then
        Internal.VariantIndexed name 1

    else
        Internal.VariantIndexed name 0


{-| A font variant might have multiple versions within the font.
In these cases we need to specify the index of the version we want.
-}
indexed :: String -> Int -> Variant
indexed name on =
    Internal.VariantIndexed name on
