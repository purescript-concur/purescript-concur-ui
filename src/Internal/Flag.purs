module Internal.Flag
    ( Field --(..)
    , Flag --(..)
    , active
    , add
    , alignBottom
    , alignRight
    , behind
    , bgColor
    , bgGradient
    , bgImage
    , borderColor
    , borderRound
    , borderStyle
    , borderWidth
    , centerX
    , centerY
    , cursor
    -- , flag
    , focus
    , fontAlignment
    , fontColor
    , fontFamily
    , fontSize
    , fontVariant
    , fontWeight
    , gridPosition
    , gridTemplate
    , height
    , heightBetween
    , heightContent
    , heightFill
    , heightTextAreaContent
    , hover
    , letterSpacing
    , merge
    , moveX
    , moveY
    , none
    , overflow
    , padding
    , present
    , rotate
    , scale
    , shadows
    , spacing
    , transparency
    , txtShadows
    -- , value
    , width
    , widthBetween
    , widthContent
    , widthFill
    , wordSpacing
    , xAlign
    , yAlign
    ) where

import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Data.Set (Set)
import Data.Set as Set

newtype Field = Field (Set Flag)

none :: Field
none = Field Set.empty

{-| If the query is in the truth, return True
-}
present :: Flag -> Field -> Boolean
present myFlag (Field set) = Set.member myFlag set

{-| Add a flag to a field.
-}
add :: Flag -> Field -> Field
add myFlag (Field set) =
    Field (Set.insert myFlag set)


{-| Generally you want to use `add`, which keeps a distinction between Fields and Flags.

Merging will combine two fields

-}
merge :: Field -> Field -> Field
merge (Field s1) (Field s2) =
    Field (Set.union s1 s2)



{- Used for Style invalidation -}

data Flag
  = Transparency
  | Padding
  | Spacing
  | FontSize
  | FontFamily
  | Width
  | Height
  | BgColor
  | BgImage
  | BgGradient
  | BorderStyle
  | FontAlignment
  | FontWeight
  | FontColor
  | WordSpacing
  | LetterSpacing
  | BorderRound
  | TxtShadows
  | Shadows
  | Overflow
  | Cursor
  | Scale
  | Rotate
  | MoveX
  | MoveY
  | BorderWidth
  | BorderColor
  | YAlign
  | XAlign
  | Focus
  | Active
  | Hover
  | GridTemplate
  | GridPosition
  | HeightContent
  | HeightFill
  | WidthContent
  | WidthFill
  | AlignRight
  | AlignBottom
  | CenterX
  | CenterY
  | WidthBetween
  | HeightBetween
  | Behind
  | HeightTextAreaContent
  | FontVariant

derive instance eqFlag :: Eq Flag
derive instance ordFlag :: Ord Flag

transparency :: Flag
transparency =
  Transparency


padding :: Flag
padding =
  Padding


spacing :: Flag
spacing =
  Spacing


fontSize :: Flag
fontSize =
  FontSize


fontFamily :: Flag
fontFamily =
  FontFamily


width :: Flag
width =
  Width


height :: Flag
height =
  Height


bgColor :: Flag
bgColor =
  BgColor


bgImage :: Flag
bgImage =
  BgImage


bgGradient :: Flag
bgGradient =
  BgGradient


borderStyle :: Flag
borderStyle =
  BorderStyle


fontAlignment :: Flag
fontAlignment =
  FontAlignment


fontWeight :: Flag
fontWeight =
  FontWeight


fontColor :: Flag
fontColor =
  FontColor


wordSpacing :: Flag
wordSpacing =
  WordSpacing


letterSpacing :: Flag
letterSpacing =
  LetterSpacing


borderRound :: Flag
borderRound =
  BorderRound


txtShadows :: Flag
txtShadows =
  TxtShadows


shadows :: Flag
shadows =
  Shadows


overflow :: Flag
overflow =
  Overflow


cursor :: Flag
cursor =
  Cursor


scale :: Flag
scale =
  Scale


rotate :: Flag
rotate =
  Rotate


moveX :: Flag
moveX =
  MoveX


moveY :: Flag
moveY =
  MoveY


borderWidth :: Flag
borderWidth =
  BorderWidth


borderColor :: Flag
borderColor =
  BorderColor


yAlign :: Flag
yAlign =
  YAlign


xAlign :: Flag
xAlign =
  XAlign


focus :: Flag
focus =
  Focus


active :: Flag
active =
  Active


hover :: Flag
hover =
  Hover


gridTemplate :: Flag
gridTemplate =
  GridTemplate


gridPosition :: Flag
gridPosition =
  GridPosition



{- Notes -}


heightContent :: Flag
heightContent =
  HeightContent


heightFill :: Flag
heightFill =
  HeightFill


widthContent :: Flag
widthContent =
  WidthContent


widthFill :: Flag
widthFill =
  WidthFill


alignRight :: Flag
alignRight =
  AlignRight


alignBottom :: Flag
alignBottom =
  AlignBottom


centerX :: Flag
centerX =
  CenterX


centerY :: Flag
centerY =
  CenterY


widthBetween :: Flag
widthBetween =
  WidthBetween


heightBetween :: Flag
heightBetween =
  HeightBetween


behind :: Flag
behind =
  Behind


heightTextAreaContent :: Flag
heightTextAreaContent =
  HeightTextAreaContent


fontVariant :: Flag
fontVariant =
  FontVariant
