module Internal.Flag
    ( Field(..)
    , Flag(..)
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
    , flag
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
    , value
    , width
    , widthBetween
    , widthContent
    , widthFill
    , wordSpacing
    , xAlign
    , yAlign
    ) where

import Data.Eq (class Eq, (==))
import Data.Field ((+), (/))
import Data.Int (round, toNumber)
import Data.Int.Bits as Bitwise
import Data.Ord ((>))
import Data.Ring ((-))
import Math (ln2, log)


data Field
    = Field Int Int


data Flag
    = Flag Int
    | Second Int

derive instance eqFlag :: Eq Flag

none :: Field
none =
    Field 0 0

log2 :: Number -> Number
log2 x = log x / ln2

value :: Flag -> Int
value myFlag =
    case myFlag of
        Flag first ->
            round (log2 (toNumber first))

        Second second ->
            round (log2 (toNumber second)) + 32


{-| If the query is in the truth, return True
-}
present :: Flag -> Field -> Boolean
present myFlag (Field fieldOne fieldTwo) =
    case myFlag of
        Flag first ->
            Bitwise.and first fieldOne == first

        Second second ->
            Bitwise.and second fieldTwo == second


{-| Add a flag to a field.
-}
add :: Flag -> Field -> Field
add myFlag (Field one two) =
    case myFlag of
        Flag first ->
            Field (Bitwise.or first one) two

        Second second ->
            Field one (Bitwise.or second two)


{-| Generally you want to use `add`, which keeps a distinction between Fields and Flags.

Merging will combine two fields

-}
merge :: Field -> Field -> Field
merge (Field one two) (Field three four) =
    Field (Bitwise.or one three) (Bitwise.or two four)


flag :: Int -> Flag
flag i =
    if i > 31 then
        Second
            (Bitwise.shl (i - 32) 1)

    else
        Flag
            (Bitwise.shl i 1)



{- Used for Style invalidation -}


transparency :: Flag
transparency =
    flag 0


padding :: Flag
padding =
    flag 2


spacing :: Flag
spacing =
    flag 3


fontSize :: Flag
fontSize =
    flag 4


fontFamily :: Flag
fontFamily =
    flag 5


width :: Flag
width =
    flag 6


height :: Flag
height =
    flag 7


bgColor :: Flag
bgColor =
    flag 8


bgImage :: Flag
bgImage =
    flag 9


bgGradient :: Flag
bgGradient =
    flag 10


borderStyle :: Flag
borderStyle =
    flag 11


fontAlignment :: Flag
fontAlignment =
    flag 12


fontWeight :: Flag
fontWeight =
    flag 13


fontColor :: Flag
fontColor =
    flag 14


wordSpacing :: Flag
wordSpacing =
    flag 15


letterSpacing :: Flag
letterSpacing =
    flag 16


borderRound :: Flag
borderRound =
    flag 17


txtShadows :: Flag
txtShadows =
    flag 18


shadows :: Flag
shadows =
    flag 19


overflow :: Flag
overflow =
    flag 20


cursor :: Flag
cursor =
    flag 21


scale :: Flag
scale =
    flag 23


rotate :: Flag
rotate =
    flag 24


moveX :: Flag
moveX =
    flag 25


moveY :: Flag
moveY =
    flag 26


borderWidth :: Flag
borderWidth =
    flag 27


borderColor :: Flag
borderColor =
    flag 28


yAlign :: Flag
yAlign =
    flag 29


xAlign :: Flag
xAlign =
    flag 30


focus :: Flag
focus =
    flag 31


active :: Flag
active =
    flag 32


hover :: Flag
hover =
    flag 33


gridTemplate :: Flag
gridTemplate =
    flag 34


gridPosition :: Flag
gridPosition =
    flag 35



{- Notes -}


heightContent :: Flag
heightContent =
    flag 36


heightFill :: Flag
heightFill =
    flag 37


widthContent :: Flag
widthContent =
    flag 38


widthFill :: Flag
widthFill =
    flag 39


alignRight :: Flag
alignRight =
    flag 40


alignBottom :: Flag
alignBottom =
    flag 41


centerX :: Flag
centerX =
    flag 42


centerY :: Flag
centerY =
    flag 43


widthBetween :: Flag
widthBetween =
    flag 44


heightBetween :: Flag
heightBetween =
    flag 45


behind :: Flag
behind =
    flag 46


heightTextAreaContent :: Flag
heightTextAreaContent =
    flag 47


fontVariant :: Flag
fontVariant =
    flag 48
