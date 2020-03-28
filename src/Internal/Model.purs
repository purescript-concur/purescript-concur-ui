module Internal.Model
    ( Adjustment(..)
    , Aligned(..)
    , Angle
    , Attribute(..)
    , Axis(..)
    , Children(..)
    , Color(..)
    , Description(..)
    , Element(..)
    , EmbedStyle(..)
    , FocusStyle
    , Font(..)
    , Gathered
    , HAlign(..)
    , HoverSetting(..)
    , LayoutContext(..)
    , Length(..)
    , Location(..)
    , NearbyChildren(..)
    , Named(..)
    , NodeName(..)
    , Option(..)
    , OptionRecord
    , Padding(..)
    , Property(..)
    , PseudoClass(..)
    , RenderMode(..)
    , Shadow
    , Spacing(..)
    , Style(..)
    , TransformComponent(..)
    , Transformation(..)
    , VAlign(..)
    , Variant(..)
    , XYZ
    , XYVal
    , addNodeName
    , addWhen
    , alignXName
    , alignYName
    , asColumn
    , asEl
    , asGrid
    , asParagraph
    , asRow
    , asTextColumn
    , boxShadowClass
    , columnClass
    , composeTransformation
    , contextClasses
    , createElement
    , defaultOptions
    , div
    , element
    , embedKeyed
    , embedWith
    , extractSpacingAndPadding
    , filter
    , finalizeNode
    , floatClass
    , focusDefaultStyle
    , formatBoxShadow
    , formatColor
    , formatColorClass
    , formatDropShadow
    , formatTextShadow
    , gatherAttrRecursive
    , get
    , getHeight
    , getSpacing
    , getStyleName
    , getWidth
    , gridClass
    , htmlClass
    , isContent
    , lengthClassName
    -- AJ: Replaced with Functor instances
    -- , mapElem
    -- , mapAttr
    , mapAttrFromStyle
    , noStyleSheet
    , onlyStyles
    , optionsToRecord
    , paddingName
    , pageClass
    , paragraphClass
    , reduceRecursive
    , reduceStyles
    , reduceStylesRecursive
    -- , removeVoid
    , renderFontClassName
    , renderHeight
    , renderRoot
    , renderVariant
    , renderWidth
    , rootStyle
    , rowClass
    , singleClass
    , spacingName
    , tag
    , textShadowClass
    , toHtml
    , toStyleSheet
    , transformClass
    , unstyled
    -- , unwrapDecorations
    , variantName
    ) where

{-| -}

-- import Html
-- import Html.Attributes
-- import Html.Keyed
-- import VirtualDom

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Category ((<<<))
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Array ((:))
import Data.Array as Array
import Data.BooleanAlgebra (not, (&&))
import Data.Eq (class Eq, (/=), (==))
import Data.EuclideanRing ((+), (-), (/))
import Data.Foldable (foldr, maximum, minimum)
import Data.Function (const, (#), ($))
import Data.Functor (class Functor, map)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (max, min, (<), (<=), (>=))
import Data.Ring ((*))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Set as Set
import Data.Show (show)
import Data.String (joinWith, toLower) as String
import Data.String.Extra (words) as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unit (Unit)
import Data.Void (Void, absurd)
import Internal.Flag (Flag)
import Internal.Flag as Flag
import Internal.Style (classes, dot, rules) as IStyle
import React.DOM (IsDynamic(..), mkDOM)
import Unsafe.Coerce (unsafeCoerce)

-- import Json.Encode as Encode

-- AJ
-- Division, but when the denominator is zero, returns zero
zeroDiv :: Int -> Int -> Int
zeroDiv n d = if d == 0 then 0 else n / d

-- AJ
words :: String -> Array String
words = String.words

data Element msg
    = Unstyled (LayoutContext -> Widget HTML msg)
    | Styled
        { styles :: Array Style
        , html :: EmbedStyle -> LayoutContext -> Widget HTML msg
        }
    | Text String
    | Empty


data EmbedStyle
    = NoStyleSheet
    | StaticRootAndDynamic OptionRecord (Array Style)
    | OnlyDynamic OptionRecord (Array Style)


noStyleSheet :: EmbedStyle
noStyleSheet =
    NoStyleSheet


data LayoutContext
    = AsRow
    | AsColumn
    | AsEl
    | AsGrid
    | AsParagraph
    | AsTextColumn

derive instance eqLayoutContext :: Eq LayoutContext

data Aligned
    = Unaligned
    | Aligned (Maybe HAlign) (Maybe VAlign)

derive instance eqAligned :: Eq Aligned

data HAlign
    = Left
    | CenterX
    | Right

derive instance eqHAlign :: Eq HAlign

data VAlign
    = Top
    | CenterY
    | Bottom

derive instance eqVAlign :: Eq VAlign

data Style
    = Style String (Array Property)
      --       class  prop   val
    | FontFamily String (Array Font)
    | FontSize Int
      -- classname, prop, value
    | Single String String String
    | Colored String String Color
    | SpacingStyle String Int Int
    | BorderWidth String Int Int Int Int
    | PaddingStyle String Int Int Int Int
    | GridTemplateStyle
        { spacing :: XYVal Length
        , columns :: Array Length
        , rows :: Array Length
        }
    | GridPosition
        { row :: Int
        , col :: Int
        , width :: Int
        , height :: Int
        }
    | Transform Transformation
    | PseudoSelector PseudoClass (Array Style)
    | Transparency String Number
    | Shadows String String

derive instance eqStyle :: Eq Style

data Transformation
    = Untransformed
    | Moved XYZ
      --              translate, scale, rotate
    | FullTransform XYZ XYZ XYZ Angle

derive instance eqTransformation :: Eq Transformation

data PseudoClass
    = Focus
    | Hover
    | Active

derive instance eqPseudoClass :: Eq PseudoClass

{-| -}
type Adjustment =
    { capital :: Number
    , lowercase :: Number
    , baseline :: Number
    , descender :: Number
    }

data Font
    = Serif
    | SansSerif
    | Monospace
    | Typeface String
    | ImportFont String String
    | FontWith
        { name :: String
        , adjustment :: Maybe Adjustment
        , variants :: Array Variant
        }
derive instance eqFont :: Eq Font

data Variant
    = VariantActive String
    | VariantOff String
    | VariantIndexed String Int

derive instance eqVariant :: Eq Variant

renderVariant :: Variant -> String
renderVariant var =
    case var of
        VariantActive name ->
            "\"" <> name <> "\""

        VariantOff name ->
            "\"" <> name <> "\" 0"

        VariantIndexed name index ->
            "\"" <> name <> "\" " <> show index

variantName :: Variant -> String
variantName var =
    case var of
        VariantActive name ->
            name

        VariantOff name ->
            name <> "-0"

        VariantIndexed name index ->
            name <> "-" <> show index


renderVariants :: Font -> Maybe String
renderVariants typeface =
    case typeface of
        FontWith font ->
            Just (String.joinWith ", " (map renderVariant font.variants))

        _ ->
            Nothing

isSmallCaps :: Variant -> Boolean
isSmallCaps var =
    case var of
        VariantActive name ->
            name == "smcp"

        VariantOff name ->
            false

        VariantIndexed name index ->
            name == "smcp" && index == 1


hasSmallCaps :: Font -> Boolean
hasSmallCaps typeface =
    case typeface of
        FontWith font ->
            Array.any isSmallCaps font.variants

        _ ->
            false


data Property
    = Property String String

derive instance eqProperty :: Eq Property


type XYZ =
    -- A
    { x::Number, y::Number, z::Number }


type Angle =
    Number


data Attribute aligned msg
    = NoAttribute
    | Attr (P.ReactProps msg)
    | Describe Description
      -- invalidation key and literal class
    | Class Flag String
      -- invalidation key "border-color" as opposed to "border-color-10-10-10" that will be the key for the class
    | StyleClass Flag Style
    | AlignY VAlign
    | AlignX HAlign
    | Width Length
    | Height Length
    | Nearby Location (Element msg)
    | TransformComponent Flag TransformComponent


data TransformComponent
    = MoveX Number
    | MoveY Number
    | MoveZ Number
    | MoveXYZ XYZ
    | Rotate XYZ Number
    | Scale XYZ


data Description
    = Main
    | Navigation
      -- | Search
    | ContentInfo
    | Complementary
    | Heading Int
    | Label String
    | LivePolite
    | LiveAssertive
    | Button
    | Paragraph


data Length
    = Px Int
    | Content
    | Fill Int
    | Min Int Length
    | Max Int Length

derive instance eqLength :: Eq Length

data Axis
    = XAxis
    | YAxis
    | AllAxis

derive instance eqAxis :: Eq Axis

data Location
    = Above
    | Below
    | OnRight
    | OnLeft
    | InFront
    | Behind

derive instance eqLocation :: Eq Location

{-| -}
data Color
    = Rgba Number Number Number Number

derive instance eqColor :: Eq Color

data NodeName
    = Generic
    | NodeName String
    | Embedded String String

derive instance eqNodeName :: Eq NodeName

data NearbyChildren msg
    = NoNearbyChildren
    | ChildrenBehind (Array (Widget HTML msg))
    | ChildrenInFront (Array (Widget HTML msg))
    | ChildrenBehindAndInFront (Array (Widget HTML msg)) (Array (Widget HTML msg))

div :: NodeName
div = Generic

type Gathered msg =
    { node :: NodeName
    , attributes :: Array (P.ReactProps msg)
    , styles :: Array Style
    , children :: NearbyChildren msg
    , has :: Flag.Field
    }


htmlClass :: forall aligned msg. String -> Attribute aligned msg
htmlClass cls = Attr $ P.className cls


{-| -}
unstyled :: forall msg. Widget HTML msg -> Element msg
unstyled =
    Unstyled <<< const

-- AJ
mkNode :: forall m a. ShiftMap (Widget HTML) m => MultiAlternative m => String -> Array (P.ReactProps a) -> Array (m a) -> m a
mkNode nodeName = D.el' (mkDOM (IsDynamic false) nodeName)

-- AJ: TODO TODO TODO TODO TODO TODO TODO
-- AJ: How do I add a key (or any other attribute) to a widget *after it's already been constructed*?
toWidget :: forall a. Named a -> a
toWidget {name:k, val:w} = w

-- AJ
keyedNode :: forall a m.
  ShiftMap (Widget HTML) m => MultiAlternative m
  => String -> Array (P.ReactProps a)
  -> Array (Named (m a))
  -> m a
keyedNode nodeName attrs keyVals = mkNode nodeName attrs (map toWidget keyVals)

finalizeNode :: forall a. Flag.Field -> NodeName -> Array (P.ReactProps a) -> Children (Widget HTML a) -> EmbedStyle -> LayoutContext -> Widget HTML a
finalizeNode has node attributes children embedMode parentContext =
    let
        createNode nodeName attrs =
            case children of
                Keyed keyed ->
                    keyedNode nodeName
                        attrs
                        (case embedMode of
                            NoStyleSheet ->
                                keyed

                            OnlyDynamic opts styles ->
                                embedKeyed false opts styles keyed

                            StaticRootAndDynamic opts styles ->
                                embedKeyed true opts styles keyed
                        )

                Unkeyed unkeyed ->
                    (case nodeName of
                        "div" ->
                            D.div

                        "p" ->
                            D.p

                        _ ->
                            mkNode nodeName
                    )
                        attrs
                        (case embedMode of
                            NoStyleSheet ->
                                unkeyed

                            OnlyDynamic opts styles ->
                                embedWith false opts styles unkeyed

                            StaticRootAndDynamic opts styles ->
                                embedWith true opts styles unkeyed
                        )

        html =
            case node of
                Generic ->
                    createNode "div" attributes

                NodeName nodeName ->
                    createNode nodeName attributes

                Embedded nodeName internal ->
                    mkNode nodeName
                        attributes
                        [ createNode internal
                            [ P.className
                                (IStyle.classes.any <> " " <> IStyle.classes.single)
                            ]
                        ]
    in
    case parentContext of
        AsRow ->
            if Flag.present Flag.widthFill has && not (Flag.present Flag.widthBetween has) then
                html

            else if Flag.present Flag.alignRight has then
                D.u
                    [ P.className
                        (String.joinWith " "
                            [ IStyle.classes.any
                            , IStyle.classes.single
                            , IStyle.classes.container
                            , IStyle.classes.contentCenterY
                            , IStyle.classes.alignContainerRight
                            ]
                        )
                    ]
                    [ html ]

            else if Flag.present Flag.centerX has then
                D.s
                    [ P.className
                        (String.joinWith " "
                            [ IStyle.classes.any
                            , IStyle.classes.single
                            , IStyle.classes.container
                            , IStyle.classes.contentCenterY
                            , IStyle.classes.alignContainerCenterX
                            ]
                        )
                    ]
                    [ html ]

            else
                html

        AsColumn ->
            if Flag.present Flag.heightFill has && not (Flag.present Flag.heightBetween has) then
                html

            else if Flag.present Flag.centerY has then
                D.s
                    [ P.className
                        (String.joinWith " "
                            [ IStyle.classes.any
                            , IStyle.classes.single
                            , IStyle.classes.container
                            , IStyle.classes.alignContainerCenterY
                            ]
                        )
                    ]
                    [ html ]

            else if Flag.present Flag.alignBottom has then
                D.u
                    [ P.className
                        (String.joinWith " "
                            [ IStyle.classes.any
                            , IStyle.classes.single
                            , IStyle.classes.container
                            , IStyle.classes.alignContainerBottom
                            ]
                        )
                    ]
                    [ html ]

            else
                html

        _ ->
            html


embedWith :: forall a.
  Boolean
  -> OptionRecord
  -> Array Style -> Array (Widget HTML a) -> Array (Widget HTML a)
embedWith static opts styles children =
    let
        dynamicStyleSheet =
            styles
                # arrayFoldl reduceStyles {cache:Set.empty, existing:renderFocusStyle opts.focus}
                # _.existing
                -- # reduceStylesRecursive Set.empty [ ]) --renderFocusStyle opts.focus ]
                -- # sortedReduce
                # toStyleSheet opts
    in
    if static then
        staticRoot opts
            : dynamicStyleSheet
            : children

    else
        dynamicStyleSheet
            : children

embedKeyed :: forall a.
  Boolean
  -> OptionRecord
  -> Array Style
  -> Array (Named (Widget HTML a))
  -> Array (Named (Widget HTML a))
embedKeyed static opts styles children =
    let
        dynamicStyleSheet =
            styles
                # arrayFoldl reduceStyles { cache: Set.empty, existing: renderFocusStyle opts.focus }
                # _.existing
                -- # reduceStylesRecursive Set.empty [ ]) --renderFocusStyle opts.focus ]
                -- # sortedReduce
                # toStyleSheet opts
    in
    if static then
        { name:"static-stylesheet", val:staticRoot opts }
            : { name:"dynamic-stylesheet"
               , val:dynamicStyleSheet
               }
            : children

    else
        { name:"dynamic-stylesheet"
        , val:dynamicStyleSheet
        }
            : children


reduceStylesRecursive :: Set String -> Array Style -> Array Style -> Array Style
reduceStylesRecursive cache found styles =
    case Array.uncons styles of
        Nothing ->
            found

        Just { head, tail:remaining} ->
            let
                styleName =
                    getStyleName head
            in
            if Set.member styleName cache then
                reduceStylesRecursive cache found remaining

            else
                reduceStylesRecursive (Set.insert styleName cache) (head : found) remaining

-- AJ
type Styl = {name::String, style::Style}
type Styls = { cache :: Set String, existing :: Array Style }


reduceStyles :: Style -> Styls -> Styls
reduceStyles style nevermind@{ cache, existing } =
    let
        styleName =
            getStyleName style
    in
    if Set.member styleName cache then
        nevermind

    else
        { cache: Set.insert styleName cache
        , existing: style : existing
        }


sortedReduce :: Array Style -> Array Style
sortedReduce styles =
    styles
        -- # map (\x -> ( getStyleName x, x ))
        # Array.sortWith getStyleName
        # reduceRecursiveCalcName []


reduceRecursiveCalcName :: Array Style -> Array Style -> Array Style
reduceRecursiveCalcName found styles =
    case Array.uncons styles of
        Nothing ->
            found

        Just {head, tail} ->
          case Array.uncons tail of
            Nothing -> Array.cons head found
            Just {head:other, tail:remaining} ->
              if head /= other then
                  reduceRecursiveCalcName (Array.cons head found) (Array.cons other remaining)
              else
                  reduceRecursiveCalcName found (Array.cons other remaining)



reduceRecursive :: Array Style -> Array Styl -> Array Style
reduceRecursive found styles =
    case Array.uncons styles of
        Nothing ->
            found

        Just {head: {name,style}, tail} ->
          case Array.uncons tail of
            Nothing -> Array.cons style found
            Just {head:{name:otherName, style:other}, tail:remaining} ->
              if name /= otherName then
                  reduceRecursive (Array.cons style found) (Array.cons {name:otherName, style:other} remaining)
              else
                  reduceRecursive found  (Array.cons {name:otherName, style:other} remaining)


addNodeName :: String -> NodeName -> NodeName
addNodeName newNode old =
    case old of
        Generic ->
            NodeName newNode

        NodeName name ->
            Embedded name newNode

        Embedded x y ->
            Embedded x y


alignXName :: HAlign -> String
alignXName align =
    case align of
        Left ->
            IStyle.classes.alignedHorizontally <> " " <> IStyle.classes.alignLeft

        Right ->
            IStyle.classes.alignedHorizontally <> " " <> IStyle.classes.alignRight

        CenterX ->
            IStyle.classes.alignedHorizontally <> " " <> IStyle.classes.alignCenterX


alignYName :: VAlign -> String
alignYName align =
    case align of
        Top ->
            IStyle.classes.alignedVertically <> " " <> IStyle.classes.alignTop

        Bottom ->
            IStyle.classes.alignedVertically <> " " <> IStyle.classes.alignBottom

        CenterY ->
            IStyle.classes.alignedVertically <> " " <> IStyle.classes.alignCenterY

transformClass :: Transformation -> Maybe String
transformClass transform =
    case transform of
        Untransformed ->
            Nothing

        Moved { x, y, z } ->
            Just $
                "mv-"
                    <> floatClass x
                    <> "-"
                    <> floatClass y
                    <> "-"
                    <> floatClass z

        FullTransform { x:tx, y:ty, z:tz } {x:sx, y:sy, z:sz} {x:ox, y:oy, z:oz} angle ->
            Just $
                "tfrm-"
                    <> floatClass tx
                    <> "-"
                    <> floatClass ty
                    <> "-"
                    <> floatClass tz
                    <> "-"
                    <> floatClass sx
                    <> "-"
                    <> floatClass sy
                    <> "-"
                    <> floatClass sz
                    <> "-"
                    <> floatClass ox
                    <> "-"
                    <> floatClass oy
                    <> "-"
                    <> floatClass oz
                    <> "-"
                    <> floatClass angle


transformValue :: Transformation -> Maybe String
transformValue transform =
    case transform of
        Untransformed ->
            Nothing

        Moved { x, y, z } ->
            Just $
                "translate3d("
                    <> show x
                    <> "px, "
                    <> show y
                    <> "px, "
                    <> show z
                    <> "px)"

        FullTransform { x:tx, y:ty, z:tz } { x:sx, y:sy, z:sz } { x:ox, y:oy, z:oz } angle ->
            let
                translate =
                    "translate3d("
                        <> show tx
                        <> "px, "
                        <> show ty
                        <> "px, "
                        <> show tz
                        <> "px)"

                scale =
                    "scale3d("
                        <> show sx
                        <> ", "
                        <> show sy
                        <> ", "
                        <> show sz
                        <> ")"

                rotate =
                    "rotate3d("
                        <> show ox
                        <> ", "
                        <> show oy
                        <> ", "
                        <> show oz
                        <> ", "
                        <> show angle
                        <> "rad)"
            in
            Just $ translate <> " " <> scale <> " " <> rotate

composeTransformation :: Transformation -> TransformComponent -> Transformation
composeTransformation transform component =
    case transform of
        Untransformed ->
            case component of
                MoveX x ->
                    Moved { x, y:0.0, z:0.0 }

                MoveY y ->
                    Moved { x:0.0, y, z:0.0 }

                MoveZ z ->
                    Moved { x:0.0, y:0.0, z }

                MoveXYZ xyz ->
                    Moved xyz

                Rotate xyz angle ->
                    FullTransform { x:0.0, y:0.0, z:0.0 } { x:1.0, y:1.0, z:1.0 } xyz angle

                Scale xyz ->
                    FullTransform { x:0.0, y:0.0, z:0.0 } xyz { x:0.0, y:0.0, z:1.0 } 0.0

        Moved (moved@{ x, y, z }) ->
            case component of
                MoveX newX ->
                    Moved { x:newX, y, z }

                MoveY newY ->
                    Moved { x, y:newY, z }

                MoveZ newZ ->
                    Moved { x, y, z:newZ }

                MoveXYZ xyz ->
                    Moved xyz

                Rotate xyz angle ->
                    FullTransform moved { x:1.0, y:1.0, z:1.0 } xyz angle

                Scale scale ->
                    FullTransform moved scale { x:0.0, y:0.0, z:1.0 } 0.0

        FullTransform (moved@{ x, y, z }) scaled origin angle ->
            case component of
                MoveX newX ->
                    FullTransform { x:newX, y, z } scaled origin angle

                MoveY newY ->
                    FullTransform { x, y:newY, z } scaled origin angle

                MoveZ newZ ->
                    FullTransform { x, y, z:newZ } scaled origin angle

                MoveXYZ newMove ->
                    FullTransform newMove scaled origin angle

                Rotate newOrigin newAngle ->
                    FullTransform moved scaled newOrigin newAngle

                Scale newScale ->
                    FullTransform moved newScale origin angle

skippable :: Flag -> Style -> Boolean
skippable flag style =
    if flag == Flag.borderWidth then
        case style of
            Single _ _ val ->
                case val of
                    "0px" ->
                        true

                    "1px" ->
                        true

                    "2px" ->
                        true

                    "3px" ->
                        true

                    "4px" ->
                        true

                    "5px" ->
                        true

                    "6px" ->
                        true

                    _ ->
                        false

            _ ->
                false

    else
        case style of
            FontSize i ->
                i >= 8 && i <= 32

            PaddingStyle name t r b l ->
                t == b && t == r && t == l && t >= 0 && t <= 24

            -- SpacingStyle _ _ _ ->
            --     true
            -- FontFamily _ _ ->
            --     true
            _ ->
                false


gatherAttrRecursive :: forall aligned msg.
    String
    -> NodeName
    -> Flag.Field
    -> Transformation
    -> Array Style
    -> Array (P.ReactProps msg)
    -> NearbyChildren msg
    -> Array (Attribute aligned msg)
    -> Gathered msg
gatherAttrRecursive classes node has transform styles attrs children elementAttrs =
    case Array.uncons elementAttrs of
        Nothing ->
            case transformClass transform of
                Nothing ->
                    { attributes: P.className classes : attrs
                    , styles: styles
                    , node: node
                    , children: children
                    , has: has
                    }

                Just klass ->
                    { attributes: P.className (classes <> " " <> klass) : attrs
                    , styles: Transform transform : styles
                    , node: node
                    , children: children
                    , has: has
                    }

        Just {head:attribute, tail:remaining} ->
            case attribute of
                NoAttribute ->
                    gatherAttrRecursive classes node has transform styles attrs children remaining

                Class flag exactClassName ->
                    if Flag.present flag has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        gatherAttrRecursive (exactClassName <> " " <> classes) node (Flag.add flag has) transform styles attrs children remaining

                Attr actualAttribute ->
                    gatherAttrRecursive classes node has transform styles (actualAttribute : attrs) children remaining

                StyleClass flag style ->
                    if Flag.present flag has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else if skippable flag style then
                        gatherAttrRecursive (getStyleName style <> " " <> classes)
                            node
                            (Flag.add flag has)
                            transform
                            styles
                            attrs
                            children
                            remaining

                    else
                        gatherAttrRecursive (getStyleName style <> " " <> classes)
                            node
                            (Flag.add flag has)
                            transform
                            (style : styles)
                            attrs
                            children
                            remaining

                TransformComponent flag component ->
                    gatherAttrRecursive classes
                        node
                        (Flag.add flag has)
                        (composeTransformation transform component)
                        styles
                        attrs
                        children
                        remaining

                Width width ->
                    if Flag.present Flag.width has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        case width of
                            Px px ->
                                gatherAttrRecursive ((IStyle.classes.widthExact <> " width-px-" <> show px) <> " " <> classes)
                                    node
                                    (Flag.add Flag.width has)
                                    transform
                                    (Single ("width-px-" <> show px) "width" (show px <> "px") : styles)
                                    attrs
                                    children
                                    remaining

                            Content ->
                                gatherAttrRecursive (classes <> " " <> IStyle.classes.widthContent)
                                    node
                                    (Flag.add Flag.widthContent (Flag.add Flag.width has))
                                    transform
                                    styles
                                    attrs
                                    children
                                    remaining

                            Fill portion ->
                                if portion == 1 then
                                    gatherAttrRecursive (classes <> " " <> IStyle.classes.widthFill)
                                        node
                                        (Flag.add Flag.widthFill (Flag.add Flag.width has))
                                        transform
                                        styles
                                        attrs
                                        children
                                        remaining

                                else
                                    gatherAttrRecursive (classes <> " " <> IStyle.classes.widthFillPortion <> " width-fill-" <> show portion)
                                        node
                                        (Flag.add Flag.widthFill (Flag.add Flag.width has))
                                        transform
                                        (Single
                                            (IStyle.classes.any
                                                <> "."
                                                <> IStyle.classes.row
                                                <> " > "
                                                <> (IStyle.dot $ "width-fill-" <> show portion)
                                            )
                                            "flex-grow"
                                            (show (portion * 100000))
                                            : styles
                                        )
                                        attrs
                                        children
                                        remaining

                            _ ->
                                let
                                    { fl: addToFlags, stylStr: newClass, stylArr: newStyles } =
                                        renderWidth width
                                in
                                gatherAttrRecursive (classes <> " " <> newClass)
                                    node
                                    (Flag.merge addToFlags (Flag.add Flag.width has))
                                    transform
                                    (newStyles <> styles)
                                    attrs
                                    children
                                    remaining

                Height height ->
                    if Flag.present Flag.height has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        case height of
                            Px px ->
                                let
                                    val =
                                        show px <> "px"

                                    name =
                                        "height-px-" <> val
                                in
                                gatherAttrRecursive (IStyle.classes.heightExact <> " " <> name <> " " <> classes)
                                    node
                                    (Flag.add Flag.height has)
                                    transform
                                    (Single name "height " val : styles)
                                    attrs
                                    children
                                    remaining

                            Content ->
                                gatherAttrRecursive (IStyle.classes.heightContent <> " " <> classes)
                                    node
                                    (Flag.add Flag.heightContent (Flag.add Flag.height has))
                                    transform
                                    styles
                                    attrs
                                    children
                                    remaining

                            Fill portion ->
                                if portion == 1 then
                                    gatherAttrRecursive (IStyle.classes.heightFill <> " " <> classes)
                                        node
                                        (Flag.add Flag.heightFill (Flag.add Flag.height has))
                                        transform
                                        styles
                                        attrs
                                        children
                                        remaining

                                else
                                    gatherAttrRecursive (classes <> " " <> (IStyle.classes.heightFillPortion <> " height-fill-" <> show portion))
                                        node
                                        (Flag.add Flag.heightFill (Flag.add Flag.height has))
                                        transform
                                        (Single
                                            (IStyle.classes.any
                                                <> "."
                                                <> IStyle.classes.column
                                                <> " > "
                                                <> (IStyle.dot $ "height-fill-" <> show portion)
                                            )
                                            "flex-grow"
                                            (show (portion * 100000))
                                            : styles
                                        )
                                        attrs
                                        children
                                        remaining

                            _ ->
                                let
                                    { fl: addToFlags, stylStr: newClass, stylArr: newStyles } =
                                        renderHeight height
                                in
                                gatherAttrRecursive (classes <> " " <> newClass)
                                    node
                                    (Flag.merge addToFlags (Flag.add Flag.height has))
                                    transform
                                    (newStyles <> styles)
                                    attrs
                                    children
                                    remaining

                Describe description ->
                    case description of
                        Main ->
                            gatherAttrRecursive classes (addNodeName "main" node) has transform styles attrs children remaining

                        Navigation ->
                            gatherAttrRecursive classes (addNodeName "nav" node) has transform styles attrs children remaining

                        ContentInfo ->
                            gatherAttrRecursive classes (addNodeName "footer" node) has transform styles attrs children remaining

                        Complementary ->
                            gatherAttrRecursive classes (addNodeName "aside" node) has transform styles attrs children remaining

                        Heading i ->
                            if i <= 1 then
                                gatherAttrRecursive classes (addNodeName "h1" node) has transform styles attrs children remaining

                            else if i < 7 then
                                gatherAttrRecursive classes (addNodeName ("h" <> show i) node) has transform styles attrs children remaining

                            else
                                gatherAttrRecursive classes (addNodeName "h6" node) has transform styles attrs children remaining

                        Paragraph ->
                            -- previously we rendered a <p> tag, though apparently this invalidates the html if it has <div>s inside.
                            -- Since we can't guaranteee that there are no divs, we need another strategy.
                            -- While it's not documented in many places, there apparently is a paragraph aria role
                            -- https://github.com/w3c/aria/blob/11f85f41a5b621fdbe85fc9bcdcd270e653a48ba/common/script/roleInfo.js
                            -- Though we'll need to wait till it gets released in an official wai-aria spec to use it.
                            -- If it's used at the moment, then Lighthouse complains (likely rightfully) that role paragraph is not recognized.
                            gatherAttrRecursive
                                classes
                                node
                                has
                                transform
                                styles
                                attrs
                                children
                                remaining

                        Button ->
                            gatherAttrRecursive classes node has transform styles (P.unsafeMkProp "role" "button" : attrs) children remaining

                        Label label ->
                            gatherAttrRecursive classes node has transform styles (P.unsafeMkProp "aria-label" label : attrs) children remaining

                        LivePolite ->
                            gatherAttrRecursive classes node has transform styles (P.unsafeMkProp "aria-live" "polite" : attrs) children remaining

                        LiveAssertive ->
                            gatherAttrRecursive classes node has transform styles (P.unsafeMkProp "aria-live" "assertive" : attrs) children remaining

                Nearby location elem ->
                    let
                        newStyles =
                            case elem of
                                Empty ->
                                    styles

                                Text str ->
                                    styles

                                Unstyled html ->
                                    styles

                                Styled styled ->
                                    styles <> styled.styles
                    in
                    gatherAttrRecursive
                        classes
                        node
                        has
                        transform
                        newStyles
                        attrs
                        (addNearbyElement location elem children)
                        remaining

                AlignX x ->
                    if Flag.present Flag.xAlign has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        gatherAttrRecursive (alignXName x <> " " <> classes)
                            node
                            (has
                                # Flag.add Flag.xAlign
                                # (\flags ->
                                        case x of
                                            CenterX ->
                                                Flag.add Flag.centerX flags

                                            Right ->
                                                Flag.add Flag.alignRight flags

                                            _ ->
                                                flags
                                   )
                            )
                            transform
                            styles
                            attrs
                            children
                            remaining

                AlignY y ->
                    if Flag.present Flag.yAlign has then
                        gatherAttrRecursive classes node has transform styles attrs children remaining

                    else
                        gatherAttrRecursive (alignYName y <> " " <> classes)
                            node
                            (Flag.add Flag.yAlign has
                                # (\flags ->
                                        case y of
                                            CenterY ->
                                                Flag.add Flag.centerY flags

                                            Bottom ->
                                                Flag.add Flag.alignBottom flags

                                            _ ->
                                                flags
                                   )
                            )
                            transform
                            styles
                            attrs
                            children
                            remaining


addNearbyElement :: forall a. Location -> Element a -> NearbyChildren a -> NearbyChildren a
addNearbyElement location elem existing =
    let
        nearby =
            nearbyElement location elem
    in
    case existing of
        NoNearbyChildren ->
            case location of
                Behind ->
                    ChildrenBehind [ nearby ]

                _ ->
                    ChildrenInFront [ nearby ]

        ChildrenBehind existingBehind ->
            case location of
                Behind ->
                    ChildrenBehind (nearby : existingBehind)

                _ ->
                    ChildrenBehindAndInFront existingBehind [ nearby ]

        ChildrenInFront existingInFront ->
            case location of
                Behind ->
                    ChildrenBehindAndInFront [ nearby ] existingInFront

                _ ->
                    ChildrenInFront (nearby : existingInFront)

        ChildrenBehindAndInFront existingBehind existingInFront ->
            case location of
                Behind ->
                    ChildrenBehindAndInFront (nearby : existingBehind) existingInFront

                _ ->
                    ChildrenBehindAndInFront existingBehind (nearby : existingInFront)


nearbyElement :: forall a. Location -> Element a -> Widget HTML a
nearbyElement location elem =
    D.div
        [ P.className $
            case location of
                Above ->
                    String.joinWith " "
                        [ IStyle.classes.nearby
                        , IStyle.classes.single
                        , IStyle.classes.above
                        ]

                Below ->
                    String.joinWith " "
                        [ IStyle.classes.nearby
                        , IStyle.classes.single
                        , IStyle.classes.below
                        ]

                OnRight ->
                    String.joinWith " "
                        [ IStyle.classes.nearby
                        , IStyle.classes.single
                        , IStyle.classes.onRight
                        ]

                OnLeft ->
                    String.joinWith " "
                        [ IStyle.classes.nearby
                        , IStyle.classes.single
                        , IStyle.classes.onLeft
                        ]

                InFront ->
                    String.joinWith " "
                        [ IStyle.classes.nearby
                        , IStyle.classes.single
                        , IStyle.classes.inFront
                        ]

                Behind ->
                    String.joinWith " "
                        [ IStyle.classes.nearby
                        , IStyle.classes.single
                        , IStyle.classes.behind
                        ]
        ]
        [ case elem of
            Empty ->
                D.text ""

            Text str ->
                textElement str

            Unstyled html ->
                html asEl

            Styled styled ->
                styled.html NoStyleSheet asEl
        ]

-- AJ
type FlagStyl =
  { fl :: Flag.Field
  , stylStr :: String
  , stylArr :: Array Style
  }

renderWidth :: Length -> FlagStyl
renderWidth w =
    case w of
        Px px ->
            { fl: Flag.none
            , stylStr: IStyle.classes.widthExact <> " width-px-" <> show px
            , stylArr: [ Single ("width-px-" <> show px) "width" (show px <> "px") ]
            }

        Content ->
            { fl: Flag.add Flag.widthContent Flag.none
            , stylStr: IStyle.classes.widthContent
            , stylArr: []
            }

        Fill portion ->
            if portion == 1 then
                { fl: Flag.add Flag.widthFill Flag.none
                , stylStr: IStyle.classes.widthFill
                , stylArr: []
                }

            else
                { fl: Flag.add Flag.widthFill Flag.none
                , stylStr: IStyle.classes.widthFillPortion <> " width-fill-" <> show portion
                , stylArr: [ Single
                        (IStyle.classes.any
                            <> "."
                            <> IStyle.classes.row
                            <> " > "
                            <> (IStyle.dot $ "width-fill-" <> show portion)
                        )
                        "flex-grow"
                        (show (portion * 100000))
                  ]
                }

        Min minSize len ->
            let
                cls =
                    "min-width-"
                        <> show minSize

                style =
                    Single
                        cls
                        "min-width"
                        (show minSize <> "px")

                { fl: newFlag, stylStr: newAttrs, stylArr: newStyle } =
                    renderWidth len
            in
            { fl: Flag.add Flag.widthBetween newFlag
            , stylStr: cls <> " " <> newAttrs
            , stylArr: style : newStyle
            }

        Max maxSize len ->
            let
                cls =
                    "max-width-" <> show maxSize

                style =
                    Single cls
                        "max-width"
                        (show maxSize <> "px")

                { fl: newFlag, stylStr: newAttrs, stylArr: newStyle } =
                    renderWidth len
            in
            { fl: Flag.add Flag.widthBetween newFlag
            , stylStr: cls <> " " <> newAttrs
            , stylArr: style : newStyle
            }


renderHeight :: Length -> FlagStyl
renderHeight h =
    case h of
        Px px ->
            let
                val =
                    show px

                name =
                    "height-px-" <> val
            in
            { fl: Flag.none
            , stylStr: IStyle.classes.heightExact <> " " <> name
            , stylArr: [ Single name "height" (val <> "px") ]
            }

        Content ->
            { fl: Flag.add Flag.heightContent Flag.none
            , stylStr: IStyle.classes.heightContent
            , stylArr: []
            }

        Fill portion ->
            if portion == 1 then
                { fl: Flag.add Flag.heightFill Flag.none
                , stylStr: IStyle.classes.heightFill
                , stylArr: []
                }

            else
                { fl: Flag.add Flag.heightFill Flag.none
                , stylStr: IStyle.classes.heightFillPortion <> " height-fill-" <> show portion
                , stylArr: [ Single
                        (IStyle.classes.any
                            <> "."
                            <> IStyle.classes.column
                            <> " > "
                            <> (IStyle.dot $ "height-fill-" <> show portion)
                        )
                        "flex-grow"
                        (show (portion * 100000))
                  ]
                }

        Min minSize len ->
            let
                cls =
                    "min-height-"
                        <> show minSize

                style =
                    Single
                        cls
                        "min-height"
                        (show minSize <> "px")

                { fl: newFlag, stylStr: newAttrs, stylArr:newStyle } =
                    renderHeight len
            in
            { fl: Flag.add Flag.heightBetween newFlag
            , stylStr: cls <> " " <> newAttrs
            , stylArr: style : newStyle
            }

        Max maxSize len ->
            let
                cls =
                    "max-height-" <> show maxSize

                style =
                    Single cls
                        "max-height"
                        (show maxSize <> "px")

                { fl: newFlag, stylStr: newAttrs, stylArr: newStyle } =
                    renderHeight len
            in
            { fl: Flag.add Flag.heightBetween newFlag
            , stylStr: cls <> " " <> newAttrs
            , stylArr: style : newStyle
            }


rowClass :: String
rowClass =
    IStyle.classes.any <> " " <> IStyle.classes.row

columnClass :: String
columnClass =
    IStyle.classes.any <> " " <> IStyle.classes.column

singleClass :: String
singleClass =
    IStyle.classes.any <> " " <> IStyle.classes.single

gridClass :: String
gridClass =
    IStyle.classes.any <> " " <> IStyle.classes.grid


paragraphClass :: String
paragraphClass =
    IStyle.classes.any <> " " <> IStyle.classes.paragraph

pageClass :: String
pageClass =
    IStyle.classes.any <> " " <> IStyle.classes.page


contextClasses :: LayoutContext -> String
contextClasses context =
    case context of
        AsRow ->
            rowClass

        AsColumn ->
            columnClass

        AsEl ->
            singleClass

        AsGrid ->
            gridClass

        AsParagraph ->
            paragraphClass

        AsTextColumn ->
            pageClass


element :: forall msg aligned. LayoutContext -> NodeName -> Array (Attribute aligned msg) -> Children (Element msg) -> Element msg
element context node attributes children =
    attributes
        # Array.reverse
        # gatherAttrRecursive (contextClasses context) node Flag.none untransformed [] [] NoNearbyChildren
        # createElement context children

untransformed :: Transformation
untransformed =
    Untransformed


createElement :: forall msg. LayoutContext -> Children (Element msg) -> Gathered msg -> Element msg
createElement context children rendered =
    let
        gather child ( Tuple htmls existingStyles ) =
            case child of
                Unstyled html ->
                    if context == asParagraph then
                        Tuple (html context : htmls) existingStyles

                    else
                        ( Tuple (html context : htmls) existingStyles )

                Styled styled ->
                    if context == asParagraph then
                        ( Tuple (styled.html NoStyleSheet context : htmls)
                          (if Array.null existingStyles then
                              styled.styles

                           else
                             styled.styles <> existingStyles
                          )
                        )

                    else
                        ( Tuple (styled.html NoStyleSheet context : htmls)
                          ( if Array.null existingStyles then
                              styled.styles

                            else
                              styled.styles <> existingStyles
                        ))

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- interferes with css grid
                    -- Maybe we could unpack text elements in a paragraph as well,
                    -- however, embedded elements that are larger than the line height will overlap with exisitng text.
                    -- I don't think that's what we want.
                    -- if
                    --     context
                    --         == asEl
                    --         || context
                    --         == asParagraph
                    -- then
                    --     ( D.text
                    --         (if context == asParagraph then
                    --             str
                    --          else
                    --             str
                    --         )
                    --         : htmls
                    --     , existingStyles
                    --     )
                    -- else
                    ( Tuple ((if context == asEl then
                        textElementFill str

                       else
                        textElement str
                      ) : htmls)
                      existingStyles
                    )

                Empty ->
                    ( Tuple htmls existingStyles )

        gatherKeyed { name:key, val:child } ( Tuple htmls existingStyles ) =
            case child of
                Unstyled html ->
                    if context == asParagraph then
                        ( Tuple ({ name:key, val: html context} : htmls)
                          ( existingStyles
                        ))

                    else
                        ( Tuple ({ name:key, val: html context} : htmls)
                          ( existingStyles
                        ))

                Styled styled ->
                    if context == asParagraph then
                        ( Tuple
                            ( { name:key, val: styled.html NoStyleSheet context } : htmls)
                            (if Array.null existingStyles then
                                styled.styles

                              else
                                styled.styles <> existingStyles
                            )
                        )

                    else
                        (Tuple
                          ({name: key, val: styled.html NoStyleSheet context } : htmls)
                          (if Array.null existingStyles then
                            styled.styles

                          else
                            styled.styles <> existingStyles
                        ))

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- if
                    --     context
                    --         == asEl
                    --         || context
                    --         == asParagraph
                    -- then
                    --     ( ( key
                    --       , D.text
                    --             str
                    --       )
                    --         : htmls
                    --     , existingStyles
                    --     )
                    -- else
                    ( Tuple ({name:key, val:
                      (if context == asEl then
                            textElementFill str

                        else
                            textElement str
                      )} : htmls)
                      existingStyles
                    )

                Empty ->
                    (Tuple htmls existingStyles )
    in
    case children of
        Keyed keyedChildren ->
            case foldr gatherKeyed (Tuple [] [] ) keyedChildren of
                ( Tuple keyed styles ) ->
                    let
                        newStyles =
                            if Array.null styles then
                                rendered.styles

                            else
                                rendered.styles <> styles
                    in
                    case newStyles of
                        [] ->
                            Unstyled
                                (finalizeNode rendered.has
                                    rendered.node
                                    rendered.attributes
                                    (Keyed
                                        (addKeyedChildren "nearby-element-pls" keyed rendered.children)
                                    )
                                    NoStyleSheet
                                )

                        allStyles ->
                            Styled
                                { styles: allStyles
                                , html:
                                    finalizeNode
                                        rendered.has
                                        rendered.node
                                        rendered.attributes
                                        (Keyed
                                            (addKeyedChildren "nearby-element-pls" keyed rendered.children)
                                        )
                                }

        Unkeyed unkeyedChildren ->
            case foldr gather ( Tuple [] [] ) unkeyedChildren of
                ( Tuple unkeyed styles ) ->
                    let
                        newStyles =
                            if Array.null styles then
                                rendered.styles

                            else
                                rendered.styles <> styles
                    in
                    case newStyles of
                        [] ->
                            Unstyled
                                (finalizeNode
                                    rendered.has
                                    rendered.node
                                    rendered.attributes
                                    (Unkeyed (addChildren unkeyed rendered.children))
                                    NoStyleSheet
                                )

                        allStyles ->
                            Styled
                                { styles: allStyles
                                , html:
                                    finalizeNode
                                        rendered.has
                                        rendered.node
                                        rendered.attributes
                                        (Unkeyed (addChildren unkeyed rendered.children))
                                }


addChildren :: forall a. Array (Widget HTML a) -> NearbyChildren a -> Array (Widget HTML a)
addChildren existing nearbyChildren =
    case nearbyChildren of
        NoNearbyChildren ->
            existing

        ChildrenBehind behind ->
            behind <> existing

        ChildrenInFront inFront ->
            existing <> inFront

        ChildrenBehindAndInFront behind inFront ->
            behind <> existing <> inFront


addKeyedChildren :: forall a.
  String
  -> Array (Named (Widget HTML a))
  -> NearbyChildren a
  -> Array (Named (Widget HTML a))
addKeyedChildren key existing nearbyChildren =
    case nearbyChildren of
        NoNearbyChildren ->
            existing

        ChildrenBehind behind ->
            map (\x -> {name:key, val:x}) behind <> existing

        ChildrenInFront inFront ->
            existing <> map (\x -> {name:key, val:x }) inFront

        ChildrenBehindAndInFront behind inFront ->
            map (\x -> {name:key, val:x}) behind
                <> existing
                <> map (\x -> {name:key, val:x}) inFront


unit :: Int
unit = 0


defaultOptions :: OptionRecord
defaultOptions =
    { hover: AllowHover
    , focus: focusDefaultStyle
    , mode: Layout
    }


staticRoot :: forall msg. OptionRecord -> Widget HTML msg
staticRoot opts =
    case opts.mode of
        Layout ->
            -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
            D.div
                []
                [ D.style [] [ D.text IStyle.rules ] ]

        NoStaticStyleSheet ->
            D.text ""

        WithVirtualCss ->
            mkNode "elm-ui-static-rules" [ P.unsafeMkProp "rules" (JString IStyle.rules) ] []


addWhen :: forall a. Boolean -> a -> Array a -> Array a
addWhen ifThis x to =
    if ifThis then
        x : to
    else
        to


{-| TODO:

This doesn't reduce equivalent attributes completely.

-}
filter :: forall aligned msg. Array (Attribute aligned msg) -> Array (Attribute aligned msg)
filter attrs =
    Tuple.fst $
        foldr
            (\x (Tuple found has ) ->
                case x of
                    NoAttribute ->
                        (Tuple found has )

                    Class key _ ->
                        (Tuple (x : found) has )

                    Attr attr ->
                        (Tuple (x : found) has )

                    StyleClass _ style ->
                        (Tuple (x : found) has )

                    Width width ->
                        if Set.member "width" has then
                            Tuple found has

                        else
                            Tuple (x : found) (Set.insert "width" has )

                    Height height ->
                        if Set.member "height" has then
                            (Tuple found has )

                        else
                            (Tuple (x : found) (Set.insert "height" has ))

                    Describe description ->
                        if Set.member "described" has then
                            (Tuple found has )

                        else
                            (Tuple (x : found) (Set.insert "described" has ))

                    Nearby location elem ->
                        (Tuple (x : found) (has ))

                    AlignX _ ->
                        if Set.member "align-x" has then
                            (Tuple found has )

                        else
                            (Tuple (x : found) (Set.insert "align-x" has ))

                    AlignY _ ->
                        if Set.member "align-y" has then
                            (Tuple found has )

                        else
                            (Tuple (x : found) (Set.insert "align-y" has ))

                    TransformComponent _ _ ->
                        if Set.member "transform" has then
                            (Tuple found has )

                        else
                            (Tuple (x : found) (Set.insert "transform" has ))
            )
            (Tuple ([]) (Set.empty ))
            attrs


isContent :: Length -> Boolean
isContent len =
    case len of
        Content ->
            true

        Max _ l ->
            isContent l

        Min _ l ->
            isContent l

        _ ->
            false


get :: forall aligned msg. Array (Attribute aligned msg) -> (Attribute aligned msg -> Boolean) -> Array (Attribute aligned msg)
get attrs isAttr =
    attrs
        # filter
        # foldr
            (\x found ->
                if isAttr x then
                    x : found

                else
                    found
            )
            []


data Spacing
    = Spaced String Int Int


data Padding
    = Padding String Int Int Int Int


extractSpacingAndPadding :: forall aligned msg. Array (Attribute aligned msg) -> Tuple (Maybe Padding) (Maybe Spacing)
extractSpacingAndPadding attrs =
    foldr
        (\attr ( Tuple pad spacing ) ->
            Tuple ( case pad of
                Just x ->
                    pad

                Nothing ->
                    case attr of
                        StyleClass _ (PaddingStyle name t r b l) ->
                            Just (Padding name t r b l)

                        _ ->
                            Nothing
            ) (case spacing of
                Just x ->
                    spacing

                Nothing ->
                    case attr of
                        StyleClass _ (SpacingStyle name x y) ->
                            Just (Spaced name x y)

                        _ ->
                            Nothing
          )
        )
        (Tuple Nothing Nothing )
        attrs


getSpacing :: forall aligned msg. Array (Attribute aligned msg) -> (Tuple Int Int ) -> (Tuple Int Int )
getSpacing attrs def =
    attrs
        # foldr
            (\attr acc ->
                case acc of
                    Just x ->
                        Just x

                    Nothing ->
                        case attr of
                            StyleClass _ (SpacingStyle _ x y) ->
                                Just ( Tuple x y )

                            _ ->
                                Nothing
            )
            Nothing
        # fromMaybe def


getWidth :: forall aligned msg. Array (Attribute aligned msg) -> Maybe Length
getWidth attrs =
    attrs
        # foldr
            (\attr acc ->
                case acc of
                    Just x ->
                        Just x

                    Nothing ->
                        case attr of
                            Width len ->
                                Just len

                            _ ->
                                Nothing
            )
            Nothing


getHeight :: forall aligned msg. Array (Attribute aligned msg) -> Maybe Length
getHeight attrs =
    attrs
        # foldr
            (\attr acc ->
                case acc of
                    Just x ->
                        Just x

                    Nothing ->
                        case attr of
                            Height len ->
                                Just len

                            _ ->
                                Nothing
            )
            Nothing


textElementClasses :: String
textElementClasses =
    IStyle.classes.any
        <> " "
        <> IStyle.classes.text
        <> " "
        <> IStyle.classes.widthContent
        <> " "
        <> IStyle.classes.heightContent


textElement :: forall msg. String -> Widget HTML msg
textElement str =
    D.div
        [ P.className
            textElementClasses
        ]
        [ D.text str ]


textElementFillClasses :: String
textElementFillClasses =
    IStyle.classes.any
        <> " "
        <> IStyle.classes.text
        <> " "
        <> IStyle.classes.widthFill
        <> " "
        <> IStyle.classes.heightFill


textElementFill :: forall msg. String -> Widget HTML msg
textElementFill str =
    D.div [ P.className textElementFillClasses ] [ D.text str ]


-- AJ
data Children x
    = Unkeyed (Array x)
    | Keyed (Array ( Named x ))


toHtml :: forall a. (Array Style -> EmbedStyle) -> Element a -> Widget HTML a
toHtml mode el =
    case el of
        Unstyled html ->
            html asEl

        Styled { styles, html } ->
            html (mode styles) asEl

        Text text ->
            textElement text

        Empty ->
            textElement ""


{-| -}
renderRoot :: forall aligned msg. Array Option -> Array (Attribute aligned msg) -> Element msg -> Widget HTML msg
renderRoot optionArray attributes child =
    let
        options =
            optionsToRecord optionArray

        embedStyle =
            case options.mode of
                NoStaticStyleSheet ->
                    OnlyDynamic options

                _ ->
                    StaticRootAndDynamic options
    in
    element asEl div attributes (Unkeyed [ child ])
        # toHtml embedStyle


data RenderMode
    = Layout
    | NoStaticStyleSheet
    | WithVirtualCss


type OptionRecord =
    { hover :: HoverSetting
    , focus :: FocusStyle
    , mode :: RenderMode
    }


data HoverSetting
    = NoHover
    | AllowHover
    | ForceHover


data Option
    = HoverOption HoverSetting
    | FocusStyleOption FocusStyle
    | RenderModeOption RenderMode


type FocusStyle =
    { borderColor :: Maybe Color
    , shadow :: Maybe Shadow
    , backgroundColor :: Maybe Color
    }

-- AJ
type XYVal a = {xval :: a, yval:: a}

-- AJ
-- AJ: Functor instance for records?
mapXYVal :: forall a b. (a -> b) -> XYVal a -> XYVal b
mapXYVal fn {xval, yval} = {xval: fn xval, yval: fn yval}

type Shadow =
    { color :: Color
    , offset :: XYVal Number
    -- Int or Number???
    , blur :: Number
    , size :: Number
    }

rootStyle :: forall msg aligned. Array (Attribute aligned msg)
rootStyle =
    let
        families =
            [ Typeface "Open Sans"
            , Typeface "Helvetica"
            , Typeface "Verdana"
            , SansSerif
            ]
    in
    [ StyleClass Flag.bgColor (Colored ("bg-" <> formatColorClass (Rgba 1.0 1.0 1.0 0.0)) "background-color" (Rgba 1.0 1.0 1.0 0.0))
    , StyleClass Flag.fontColor (Colored ("fc-" <> formatColorClass (Rgba 0.0 0.0 0.0 1.0)) "color" (Rgba 0.0 0.0 0.0 1.0))
    , StyleClass Flag.fontSize (FontSize 20)
    , StyleClass Flag.fontFamily $
        FontFamily (arrayFoldl renderFontClassName "font-" families)
            families
    ]


renderFontClassName :: Font -> String -> String
renderFontClassName font current =
    current
        <> (case font of
                Serif ->
                    "serif"

                SansSerif ->
                    "sans-serif"

                Monospace ->
                    "monospace"

                Typeface name ->
                    name
                        # String.toLower
                        # words
                        # String.joinWith "-"

                ImportFont name url ->
                    name
                        # String.toLower
                        # words
                        # String.joinWith "-"

                FontWith { name } ->
                    name
                        # String.toLower
                        # words
                        # String.joinWith "-"
           )



renderFocusStyle :: FocusStyle -> Array Style
renderFocusStyle focus =
    [ Style (IStyle.dot IStyle.classes.focusedWithin <> ":focus-within")
        (Array.catMaybes
            [ map (\color -> Property "border-color" (formatColor color)) focus.borderColor
            , map (\color -> Property "background-color" (formatColor color)) focus.backgroundColor
            , focus.shadow # map \shadow ->
                    Property "box-shadow" (formatBoxShadow false shadow)
            , Just $ Property "outline" "none"
            ]
        )
    , Style (IStyle.dot IStyle.classes.any <> ":focus .focusable, " <> IStyle.dot IStyle.classes.any <> ".focusable:focus")
        (Array.catMaybes
            [ map (\color -> Property "border-color" (formatColor color)) focus.borderColor
            , map (\color -> Property "background-color" (formatColor color)) focus.backgroundColor
            , map
                (\shadow ->
                    Property "box-shadow" (formatBoxShadow false shadow)
                )
                focus.shadow
            , Just $ Property "outline" "none"
            ]
        )
    ]


focusDefaultStyle :: { backgroundColor :: Maybe Color, borderColor :: Maybe Color, shadow :: Maybe Shadow }
focusDefaultStyle =
    { backgroundColor: Nothing
    , borderColor: Nothing
    , shadow:
        Just
            { color:
                Rgba (155.0 / 255.0) (203.0 / 255.0) 1.0 1.0
            , offset: {xval: 0.0, yval: 0.0}
            , blur: 0.0
            , size: 3.0
            }
    }


optionsToRecord :: Array Option -> OptionRecord
optionsToRecord options =
    let
        combine opt record =
            case opt of
                HoverOption hoverable ->
                    case record.hover of
                        Nothing ->
                            record { hover = Just hoverable }

                        _ ->
                            record

                FocusStyleOption focusStyle ->
                    case record.focus of
                        Nothing ->
                            record { focus = Just focusStyle }

                        _ ->
                            record

                RenderModeOption renderMode ->
                    case record.mode of
                        Nothing ->
                            record { mode = Just renderMode }

                        _ ->
                            record

        andFinally record =
            { hover:
                case record.hover of
                    Nothing ->
                        AllowHover

                    Just hoverable ->
                        hoverable
            , focus:
                case record.focus of
                    Nothing ->
                        focusDefaultStyle

                    Just focusable ->
                        focusable
            , mode:
                case record.mode of
                    Nothing ->
                        Layout

                    Just actualMode ->
                        actualMode
            }
    in
    andFinally $
        foldr combine
            { hover: Nothing
            , focus: Nothing
            , mode: Nothing
            }
            options


toStyleSheet :: forall msg. OptionRecord -> Array Style -> Widget HTML msg
toStyleSheet options styleSheet =
    case options.mode of
        Layout ->
            -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
            mkNode "div"
                []
                [ mkNode "style"
                    []
                    [ D.text (toStyleSheetString options styleSheet) ]
                ]

        NoStaticStyleSheet ->
            -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
            mkNode "div"
                []
                [ mkNode "style"
                    []
                    [ D.text (toStyleSheetString options styleSheet) ]
                ]

        WithVirtualCss ->
            mkNode "elm-ui-rules"
                [ P.unsafeMkProp "rules"
                    (encodeStyles options styleSheet)
                ]
                []


renderTopLevelValues :: Array (Tuple String (Array Font)) -> String
renderTopLevelValues rules =
    let
        withImport font =
            case font of
                ImportFont _ url ->
                    Just ("@import url('" <> url <> "');")

                -- FontWith with ->
                --     case with.url of
                --         Just x ->
                --             Just ("@import url('" <> x <> "');")
                --         Nothing ->
                --             Nothing
                _ ->
                    Nothing

        allNames =
            map Tuple.fst rules

        fontImports ( Tuple name typefaces ) =
            let
                imports =
                    String.joinWith "\n" (Array.catMaybes $ map withImport typefaces)
            in
            imports

        fontAdjustments ( Tuple name typefaces ) =
            case typefaceAdjustment typefaces of
                Nothing ->
                    String.joinWith ""
                        (map (renderNullAdjustmentRule name) allNames)

                Just adjustment ->
                    String.joinWith ""
                        (map (renderFontAdjustmentRule name adjustment) allNames)
    in
    String.joinWith "\n" (map fontImports rules)
        <> String.joinWith "\n" (map fontAdjustments rules)


renderNullAdjustmentRule :: String -> String -> String
renderNullAdjustmentRule fontToAdjust otherFontName =
    let
        name =
            if fontToAdjust == otherFontName then
                fontToAdjust

            else
                otherFontName <> " ." <> fontToAdjust
    in
    String.joinWith " "
        [ bracket
            ("."
                <> name
                <> "."
                <> IStyle.classes.sizeByCapital
                <> ", "
                <> "."
                <> name
                <> " ."
                <> IStyle.classes.sizeByCapital
            )
            [ (Tuple "line-height" "1" )
            ]
        , bracket
            ("."
                <> name
                <> "."
                <> IStyle.classes.sizeByCapital
                <> "> ."
                <> IStyle.classes.text
                <> ", ."
                <> name
                <> " ."
                <> IStyle.classes.sizeByCapital
                <> " > ."
                <> IStyle.classes.text
            )
            [ (Tuple "vertical-align" "0" )
            , (Tuple "line-height" "1" )
            ]
        ]


fontRule :: String -> String -> Tuple (Array (Tuple String String)) (Array (Tuple String String)) -> Array String
fontRule name modifier ( Tuple parentAdj textAdjustment ) =
    [ bracket
        ("."
            <> name
            <> "."
            <> modifier
            <> ", "
            <> "."
            <> name
            <> " ."
            <> modifier
        )
        parentAdj
    , bracket
        ("."
            <> name
            <> "."
            <> modifier
            <> "> ."
            <> IStyle.classes.text
            <> ", ."
            <> name
            <> " ."
            <> modifier
            <> " > ."
            <> IStyle.classes.text
        )
        textAdjustment
    ]


-- AJ: WTF
renderFontAdjustmentRule :: String -> Tuple (Tuple (Array (Tuple String String)) (Array (Tuple String String))) (Tuple (Array (Tuple String String)) (Array (Tuple String String))) -> String -> String
renderFontAdjustmentRule fontToAdjust ( Tuple full capital ) otherFontName =
    let
        name =
            if fontToAdjust == otherFontName then
                fontToAdjust

            else
                otherFontName <> " ." <> fontToAdjust
    in
    String.joinWith " "
        (fontRule name IStyle.classes.sizeByCapital capital <> fontRule name IStyle.classes.fullSize full)


bracket :: String -> Array (Tuple String String) -> String
bracket selector rules =
    let
        renderPair ( Tuple name val ) =
            name <> ": " <> val <> ";"
    in
    selector <> " {" <> String.joinWith "" (map renderPair rules) <> "}"

-- AJ
type FontAdjustmentRules =
  { height :: Number
  , size :: Number
  , vertical :: Number
  }

fontAdjustmentRules ::
  FontAdjustmentRules
  -> Tuple (Array (Tuple String String)) (Array (Tuple String String))
fontAdjustmentRules converted =
    Tuple

      [ Tuple  "display" "block" ]

      [ Tuple "display" "inline-block"
      , Tuple "line-height" (show converted.height )
      , Tuple "vertical-align" (show converted.vertical <> "em" )
      , Tuple "font-size" (show converted.size <> "em" )
      ]

-- AJ: WTF
typefaceAdjustment :: Array Font -> Maybe (Tuple (Tuple (Array (Tuple String String)) (Array (Tuple String String))) (Tuple (Array (Tuple String String)) (Array (Tuple String String))))
typefaceAdjustment typefaces =
    arrayFoldl
        (\face found ->
            case found of
                Nothing ->
                    case face of
                        FontWith with ->
                            case with.adjustment of
                                Nothing ->
                                    found

                                Just adjustment ->
                                    Just $
                                        Tuple
                                          ( fontAdjustmentRules
                                            ((convertAdjustment adjustment).full)
                                          )
                                          ( fontAdjustmentRules
                                            ((convertAdjustment adjustment).capital)
                                          )

                        _ ->
                            found

                Just _ ->
                    found
        )
        Nothing
        typefaces


fontName :: Font -> String
fontName font =
    case font of
        Serif ->
            "serif"

        SansSerif ->
            "sans-serif"

        Monospace ->
            "monospace"

        Typeface name ->
            "\"" <> name <> "\""

        ImportFont name url ->
            "\"" <> name <> "\""

        FontWith { name } ->
            "\"" <> name <> "\""

topLevelValue :: Style -> Maybe (Tuple String (Array Font))
topLevelValue rule =
    case rule of
        FontFamily name typefaces ->
            Just ( Tuple name typefaces )

        _ ->
            Nothing


renderProps :: Boolean -> Property -> String -> String
renderProps force (Property key val) existing =
    if force then
        existing <> "\n  " <> key <> ": " <> val <> " !important;"

    else
        existing <> "\n  " <> key <> ": " <> val <> ";"

-- AJ
type Named a =
  { name :: String
  , val :: a
  }

-- AJ: TODO: TODO: TODO: IMPLEMENT ACTUAL JSON!
data Json
  = JObject (Array (Named Json))
  | JList (Array Json)
  | JString String
  | JInt Int
  | JNumber Number
  | JBool Boolean
-- AJ: END

encodeStyles :: OptionRecord -> Array Style -> Json
encodeStyles options stylesheet =
    stylesheet
        # map
            (\style ->
                let
                    styled =
                        renderStyleRule options style Nothing
                in
                  {name: getStyleName style, val: JList (map JString styled ) }
            )
        # JObject


toStyleSheetString :: OptionRecord -> Array Style -> String
toStyleSheetString options stylesheet =
  renderTopLevelValues topLevel <> String.joinWith "" rules
  where
    combine style rendered =
        { rules: rendered.rules <> renderStyleRule options style Nothing
        , topLevel:
            case topLevelValue style of
                Nothing ->
                    rendered.topLevel

                Just topLevel ->
                    topLevel : rendered.topLevel
        }
    {topLevel, rules} = arrayFoldl combine { topLevel: [], rules: [] } stylesheet


renderStyle :: OptionRecord -> Maybe PseudoClass -> String -> Array Property -> Array String
renderStyle options maybePseudo selector props =
    case maybePseudo of
        Nothing ->
            [ selector <> "{" <> arrayFoldl (renderProps false) "" props <> "\n}" ]

        Just pseudo ->
            case pseudo of
                Hover ->
                    case options.hover of
                        NoHover ->
                            []

                        ForceHover ->
                            [ selector <> "-hv {" <> arrayFoldl (renderProps true) "" props <> "\n}" ]

                        AllowHover ->
                            [ selector <> "-hv:hover {" <> arrayFoldl (renderProps false) "" props <> "\n}" ]

                Focus ->
                    let
                        renderedProps =
                            arrayFoldl (renderProps false) "" props
                    in
                    [ selector
                        <> "-fs:focus {"
                        <> renderedProps
                        <> "\n}"
                    , "."
                        <> IStyle.classes.any
                        <> ":focus ~ "
                        <> selector
                        <> "-fs:not(.focus)  {"
                        <> renderedProps
                        <> "\n}"
                    , "."
                        <> IStyle.classes.any
                        <> ":focus "
                        <> selector
                        <> "-fs  {"
                        <> renderedProps
                        <> "\n}"
                    , selector
                        <> "-fs:focus-within {"
                        <> renderedProps
                        <> "\n}"
                    , ".focusable-parent:focus ~ "
                        <> "."
                        <> IStyle.classes.any
                        <> " "
                        <> selector
                        <> "-fs {"
                        <> renderedProps
                        <> "\n}"
                    ]

                Active ->
                    [ selector <> "-act:active {" <> arrayFoldl (renderProps false) "" props <> "\n}" ]


renderStyleRule :: OptionRecord -> Style -> Maybe PseudoClass -> Array String
renderStyleRule options rule maybePseudo =
    case rule of
        Style selector props ->
            renderStyle options maybePseudo selector props

        Shadows name prop ->
            renderStyle options
                maybePseudo
                ("." <> name)
                [ Property "box-shadow" prop
                ]

        Transparency name transparency ->
            let
                opacity =
                    (1.0 - transparency)
                        # min 1.0
                        # max 0.0
            in
            renderStyle options
                maybePseudo
                ("." <> name)
                [ Property "opacity" (show opacity)
                ]

        FontSize i ->
            renderStyle options
                maybePseudo
                (".font-size-" <> show i)
                [ Property "font-size" (show i <> "px")
                ]

        FontFamily name typefaces ->
            let
                features =
                    typefaces
                        # map renderVariants
                        # Array.catMaybes
                        # String.joinWith ", "

                families =
                    [ Property "font-family"
                        (typefaces
                            # map fontName
                            # String.joinWith ", "
                        )
                    , Property "font-feature-settings" features
                    , Property "font-variant"
                        (if Array.any hasSmallCaps typefaces then
                            "small-caps"

                         else
                            "normal"
                        )
                    ]
            in
            renderStyle options
                maybePseudo
                ("." <> name)
                families

        Single klass prop val ->
            renderStyle options
                maybePseudo
                ("." <> klass)
                [ Property prop val
                ]

        Colored klass prop color ->
            renderStyle options
                maybePseudo
                ("." <> klass)
                [ Property prop (formatColor color)
                ]

        SpacingStyle cls x y ->
            let
                klass =
                    "." <> cls

                halfX =
                    show (toNumber x / 2.0) <> "px"

                halfY =
                    show (toNumber y / 2.0) <> "px"

                xPx =
                    show x <> "px"

                yPx =
                    show y <> "px"

                row =
                    "." <> IStyle.classes.row

                wrappedRow =
                    "." <> IStyle.classes.wrapped <> row

                column =
                    "." <> IStyle.classes.column

                page =
                    "." <> IStyle.classes.page

                paragraph =
                    "." <> IStyle.classes.paragraph

                left =
                    "." <> IStyle.classes.alignLeft

                right =
                    "." <> IStyle.classes.alignRight

                any =
                    "." <> IStyle.classes.any

                single =
                    "." <> IStyle.classes.single
            in
            Array.concat
                [ renderStyle options maybePseudo (klass <> row <> " > " <> any <> " + " <> any) [ Property "margin-left" xPx ]

                -- margins don't apply to last element of normal, unwrapped rows
                -- , renderStyle options maybePseudo (class <> row <> " > " <> any <> ":first-child") [ Property "margin" "0" ]
                -- For wrapped rows, margins always apply because we handle "canceling out" the other margins manually in the element.
                , renderStyle options
                    maybePseudo
                    (klass <> wrappedRow <> " > " <> any)
                    [ Property "margin" (halfY <> " " <> halfX)
                    ]

                -- , renderStyle options maybePseudo
                --     (class <> wrappedRow <> " > " <> any <> ":last-child")
                --     [ Property "margin-right" "0"
                --     ]
                -- columns
                , renderStyle options maybePseudo (klass <> column <> " > " <> any <> " + " <> any) [ Property "margin-top" yPx ]
                , renderStyle options maybePseudo (klass <> page <> " > " <> any <> " + " <> any) [ Property "margin-top" yPx ]
                , renderStyle options maybePseudo (klass <> page <> " > " <> left) [ Property "margin-right" xPx ]
                , renderStyle options maybePseudo (klass <> page <> " > " <> right) [ Property "margin-left" xPx ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph)
                    [ Property "line-height" ("calc(1em + " <> show y <> "px)")
                    ]
                , renderStyle options
                    maybePseudo
                    ("textarea" <> any <> klass)
                    [ Property "line-height" ("calc(1em + " <> show y <> "px)")
                    , Property "height" ("calc(100% + " <> show y <> "px)")
                    ]

                -- , renderStyle options
                --     maybePseudo
                --     (class <> paragraph <> " > " <> any)
                --     [ Property "margin-right" xPx
                --     , Property "margin-bottom" yPx
                --     ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> " > " <> left)
                    [ Property "margin-right" xPx
                    ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> " > " <> right)
                    [ Property "margin-left" xPx
                    ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> "::after")
                    [ Property "content" "''"
                    , Property "display" "block"
                    , Property "height" "0"
                    , Property "width" "0"
                    , Property "margin-top" (show ((0-1) * (y `zeroDiv` 2)) <> "px")
                    ]
                , renderStyle options
                    maybePseudo
                    (klass <> paragraph <> "::before")
                    [ Property "content" "''"
                    , Property "display" "block"
                    , Property "height" "0"
                    , Property "width" "0"
                    , Property "margin-bottom" (show ((0-1) * (y `zeroDiv` 2)) <> "px")
                    ]
                ]

        PaddingStyle cls top right bottom left ->
            let
                klass =
                    "."
                        <> cls
            in
            renderStyle options
                maybePseudo
                klass
                [ Property "padding"
                    (show top
                        <> "px "
                        <> show right
                        <> "px "
                        <> show bottom
                        <> "px "
                        <> show left
                        <> "px"
                    )
                ]

        BorderWidth cls top right bottom left ->
            let
                klass =
                    "."
                        <> cls
            in
            renderStyle options
                maybePseudo
                klass
                [ Property "border-width"
                    (show top
                        <> "px "
                        <> show right
                        <> "px "
                        <> show bottom
                        <> "px "
                        <> show left
                        <> "px"
                    )
                ]

        GridTemplateStyle template ->
            let
                klass =
                    ".grid-rows-"
                        <> String.joinWith "-" (map lengthClassName template.rows)
                        <> "-cols-"
                        <> String.joinWith "-" (map lengthClassName template.columns)
                        <> "-space-x-"
                        <> lengthClassName template.spacing.xval
                        <> "-space-y-"
                        <> lengthClassName template.spacing.yval

                ySpacing =
                    toGridLength template.spacing.yval

                xSpacing =
                    toGridLength template.spacing.xval

                toGridLength x =
                    toGridLengthHelper Nothing Nothing x

                toGridLengthHelper minimum maximum x =
                    case x of
                        Px px ->
                            show px <> "px"

                        Content ->
                            case Tuple minimum maximum of
                                Tuple Nothing Nothing ->
                                    "max-content"

                                Tuple (Just minSize) Nothing ->
                                    "minmax(" <> show minSize <> "px, " <> "max-content)"

                                Tuple Nothing (Just maxSize) ->
                                    "minmax(max-content, " <> show maxSize <> "px)"

                                Tuple (Just minSize) (Just maxSize) ->
                                    "minmax(" <> show minSize <> "px, " <> show maxSize <> "px)"

                        Fill i ->
                            case Tuple minimum maximum of
                                Tuple Nothing Nothing ->
                                    show i <> "fr"

                                Tuple (Just minSize) (Nothing ) ->
                                    "minmax(" <> show minSize <> "px, " <> show i <> "fr" <> "fr)"

                                Tuple (Nothing) (Just maxSize ) ->
                                    "minmax(max-content, " <> show maxSize <> "px)"

                                Tuple (Just minSize) (Just maxSize ) ->
                                    "minmax(" <> show minSize <> "px, " <> show maxSize <> "px)"

                        Min m len ->
                            toGridLengthHelper (Just m) maximum len

                        Max m len ->
                            toGridLengthHelper minimum (Just m) len

                msColumns =
                    template.columns
                        # map toGridLength
                        # String.joinWith ySpacing
                        # (\x -> "-ms-grid-columns: " <> x <> ";")

                msRows =
                    template.columns
                        # map toGridLength
                        # String.joinWith ySpacing
                        # (\x -> "-ms-grid-rows: " <> x <> ";")

                base =
                    klass <> "{" <> msColumns <> msRows <> "}"

                columns =
                    template.columns
                        # map toGridLength
                        # String.joinWith " "
                        # (\x -> "grid-template-columns: " <> x <> ";")

                rows =
                    template.rows
                        # map toGridLength
                        # String.joinWith " "
                        # (\x -> "grid-template-rows: " <> x <> ";")

                gapX =
                    "grid-column-gap:" <> toGridLength template.spacing.xval <> ";"

                gapY =
                    "grid-row-gap:" <> toGridLength template.spacing.yval <> ";"

                modernGrid =
                    klass <> "{" <> columns <> rows <> gapX <> gapY <> "}"

                supports =
                    "@supports (display:grid) {" <> modernGrid <> "}"
            in
            [ base
            , supports
            ]

        GridPosition position ->
            let
                klass =
                    ".grid-pos-"
                        <> show position.row
                        <> "-"
                        <> show position.col
                        <> "-"
                        <> show position.width
                        <> "-"
                        <> show position.height

                msPosition =
                    String.joinWith " "
                        [ "-ms-grid-row: "
                            <> show position.row
                            <> ";"
                        , "-ms-grid-row-span: "
                            <> show position.height
                            <> ";"
                        , "-ms-grid-column: "
                            <> show position.col
                            <> ";"
                        , "-ms-grid-column-span: "
                            <> show position.width
                            <> ";"
                        ]

                base =
                    klass <> "{" <> msPosition <> "}"

                modernPosition =
                    String.joinWith " "
                        [ "grid-row: "
                            <> show position.row
                            <> " / "
                            <> show (position.row + position.height)
                            <> ";"
                        , "grid-column: "
                            <> show position.col
                            <> " / "
                            <> show (position.col + position.width)
                            <> ";"
                        ]

                modernGrid =
                    klass <> "{" <> modernPosition <> "}"

                supports =
                    "@supports (display:grid) {" <> modernGrid <> "}"
            in
            [ base
            , supports
            ]

        PseudoSelector klass styles ->
            let
                renderPseudoRule style =
                    renderStyleRule options style (Just klass)
            in
            Array.concatMap renderPseudoRule styles

        Transform transform ->
            let
                val =
                    transformValue transform

                klass =
                    transformClass transform
            in
            case Tuple (klass) (val ) of
                Tuple (Just cls) (Just v ) ->
                    renderStyle options
                        maybePseudo
                        ("." <> cls)
                        [ Property "transform"
                            v
                        ]

                _ ->
                    []


lengthClassName :: Length -> String
lengthClassName x =
    case x of
        Px px ->
            show px <> "px"

        Content ->
            "auto"

        Fill i ->
            show i <> "fr"

        Min min len ->
            "min" <> show min <> lengthClassName len

        Max max len ->
            "max" <> show max <> lengthClassName len


formatDropShadow :: Shadow -> String
formatDropShadow shadow =
    String.joinWith " "
        [ show shadow.offset.xval <> "px"
        , show shadow.offset.yval <> "px"
        , show shadow.blur <> "px"
        , formatColor shadow.color
        ]


formatTextShadow :: Shadow -> String
formatTextShadow shadow =
    String.joinWith " "
        [ show shadow.offset.xval <> "px"
        , show shadow.offset.yval <> "px"
        , show shadow.blur <> "px"
        , formatColor shadow.color
        ]


textShadowClass :: Shadow -> String
textShadowClass shadow =
    String.joinWith ""
        [ "txt"
        , floatClass shadow.offset.xval <> "px"
        , floatClass shadow.offset.yval <> "px"
        , floatClass shadow.blur <> "px"
        , formatColorClass shadow.color
        ]

formatBoxShadow :: Boolean -> Shadow -> String
formatBoxShadow inset shadow =
    String.joinWith " " $
        Array.catMaybes
            [ if inset then
                Just "inset"
              else
                Nothing
            , Just $ show shadow.offset.xval <> "px"
            , Just $ show shadow.offset.yval <> "px"
            , Just $ show shadow.blur <> "px"
            , Just $ show shadow.size <> "px"
            , Just $ formatColor shadow.color
            ]


boxShadowClass :: Boolean -> Shadow -> String
boxShadowClass inset shadow =
    String.joinWith "" $
        [ if inset then
            "box-inset"

          else
            "box-"
        , floatClass shadow.offset.xval <> "px"
        , floatClass shadow.offset.yval <> "px"
        , floatClass shadow.blur <> "px"
        , floatClass shadow.size <> "px"
        , formatColorClass shadow.color
        ]


floatClass :: Number -> String
floatClass x =
    show (round (x * 255.0))


formatColor :: Color -> String
formatColor (Rgba red green blue alpha) =
    "rgba("
        <> show (round (red * 255.0))
        <> ("," <> show (round (green * 255.0)))
        <> ("," <> show (round (blue * 255.0)))
        <> ("," <> show alpha <> ")")


formatColorClass :: Color -> String
formatColorClass (Rgba red green blue alpha) =
    floatClass red
        <> "-"
        <> floatClass green
        <> "-"
        <> floatClass blue
        <> "-"
        <> floatClass alpha


spacingName :: Int -> Int -> String
spacingName x y =
    "spacing-" <> show x <> "-" <> show y


paddingName :: Int -> Int -> Int -> Int -> String
paddingName top right bottom left =
    "pad-"
        <> show top
        <> "-"
        <> show right
        <> "-"
        <> show bottom
        <> "-"
        <> show left


getStyleName :: Style -> String
getStyleName style =
    case style of
        Shadows name _ ->
            name

        Transparency name o ->
            name

        Style klass _ ->
            klass

        FontFamily name _ ->
            name

        FontSize i ->
            "font-size-" <> show i

        Single klass _ _ ->
            klass

        Colored klass _ _ ->
            klass

        SpacingStyle cls x y ->
            cls

        PaddingStyle cls top right bottom left ->
            cls

        BorderWidth cls top right bottom left ->
            cls

        GridTemplateStyle template ->
            "grid-rows-"
                <> String.joinWith "-" (map lengthClassName template.rows)
                <> "-cols-"
                <> String.joinWith "-" (map lengthClassName template.columns)
                <> "-space-x-"
                <> lengthClassName template.spacing.xval
                <> "-space-y-"
                <> lengthClassName template.spacing.yval

        GridPosition pos ->
            "gp grid-pos-"
                <> show pos.row
                <> "-"
                <> show pos.col
                <> "-"
                <> show pos.width
                <> "-"
                <> show pos.height

        PseudoSelector selector subStyle ->
            let
                name =
                    case selector of
                        Focus ->
                            "fs"

                        Hover ->
                            "hv"

                        Active ->
                            "act"
            in
            map
                (\sty ->
                    case getStyleName sty of
                        "" ->
                            ""

                        styleName ->
                            styleName <> "-" <> name
                )
                subStyle
                # String.joinWith " "

        Transform x ->
            fromMaybe "" (transformClass x)



{- Constants -}


asGrid :: LayoutContext
asGrid =
    AsGrid


asRow :: LayoutContext
asRow =
    AsRow


asColumn :: LayoutContext
asColumn =
    AsColumn


asEl :: LayoutContext
asEl =
    AsEl


asParagraph :: LayoutContext
asParagraph =
    AsParagraph


asTextColumn :: LayoutContext
asTextColumn =
    AsTextColumn



{- Mapping -}

instance functorElement :: Functor Element where
  map fn el =
    case el of
        Styled styled ->
            Styled
                { styles: styled.styles
                , html: \add context -> map fn $ styled.html add context
                }

        Unstyled html ->
            Unstyled (map fn <<< html)

        Text str ->
            Text str

        Empty ->
            Empty

instance functorAttr :: Functor (Attribute aligned) where
  map fn attr =
    case attr of
        NoAttribute ->
            NoAttribute

        Describe description ->
            Describe description

        AlignX x ->
            AlignX x

        AlignY y ->
            AlignY y

        Width x ->
            Width x

        Height x ->
            Height x

        Class x y ->
            Class x y

        StyleClass flag style ->
            StyleClass flag style

        Nearby location elem ->
            Nearby location (map fn elem)

        Attr htmlAttr ->
            Attr (map fn htmlAttr)

        TransformComponent fl trans ->
            TransformComponent fl trans


mapAttrFromStyle :: forall msg msg1. (msg -> msg1) -> Attribute Void msg -> Attribute Unit msg1
mapAttrFromStyle fn attr = unsafeCoerce (map fn attr)

-- AJ
--- AJ: TODO TODO TODO: FOLDL ADAPTER
-- AJ: We need functionality of foldl, but with signature of foldr
arrayFoldl :: forall a b. (a -> b -> b) -> b -> Array a -> b
arrayFoldl = foldr

unwrapDecorations :: Array (Attribute Void Void) -> Array Style
unwrapDecorations attrs =
    case arrayFoldl unwrapDecsHelper (Tuple [] Untransformed) attrs of
        ( Tuple styles transform ) ->
            Transform transform : styles


unwrapDecsHelper :: Attribute Void Void -> Tuple (Array Style) Transformation -> Tuple (Array Style) Transformation
unwrapDecsHelper attr (Tuple styles trans ) =
    case removeVoid attr of
        StyleClass _ style ->
            Tuple ( style : styles) trans

        TransformComponent flag component ->
            Tuple (styles) (composeTransformation trans component )

        _ ->
            Tuple (styles) (trans )


removeVoid :: forall msg. Attribute Void Void -> Attribute Unit msg
removeVoid style =
    mapAttrFromStyle absurd style

tag :: String -> Style -> Style
tag label style =
    case style of
        Single klass prop val ->
            Single (label <> "-" <> klass) prop val

        Colored klass prop val ->
            Colored (label <> "-" <> klass) prop val

        Style klass props ->
            Style (label <> "-" <> klass) props

        Transparency klass o ->
            Transparency (label <> "-" <> klass) o

        x ->
            x


onlyStyles :: forall aligned msg. Attribute aligned msg -> Maybe Style
onlyStyles attr =
    case attr of
        StyleClass _ style ->
            Just style

        _ ->
            Nothing



{- Font Adjustments -}


convertAdjustment :: Adjustment
  -> { capital :: FontAdjustmentRules
     , full :: FontAdjustmentRules
     }
convertAdjustment adjustment =
    let
        lineHeight =
            1.5

        base =
            lineHeight

        normalDescender =
            (lineHeight - 1.0)
                / 2.0

        oldMiddle =
            lineHeight / 2.0

        newCapitalMiddle =
            ((ascender - newBaseline) / 2.0) + newBaseline

        newFullMiddle =
            ((ascender - descender) / 2.0) + descender

        lines =
            [ adjustment.capital
            , adjustment.baseline
            , adjustment.descender
            , adjustment.lowercase
            ]

        ascender =
            fromMaybe adjustment.capital (maximum lines)

        descender =
            fromMaybe adjustment.descender (minimum lines)

        newBaseline =
            lines
                # Array.filter (\x -> x /= descender)
                # minimum
                # fromMaybe adjustment.baseline

        capitalVertical =
            1.0 - ascender

        capitalSize =
            1.0 / (ascender - newBaseline)

        fullSize =
            1.0 / (ascender - descender)

        fullVertical =
            1.0 - ascender

        -- (oldMiddle - newFullMiddle) * 2
    in
    { full:
        adjust fullSize (ascender - descender) fullVertical
    , capital:
        adjust capitalSize (ascender - newBaseline) capitalVertical
    }


adjust :: Number -> Number -> Number -> FontAdjustmentRules
adjust size height vertical =
    { vertical: vertical
    , height:
        height / size
    , size: size
    }
