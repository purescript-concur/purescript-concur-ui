module Element
    ( Element, none, text, el
    , row, wrappedRow, column
    , paragraph, textColumn
    , Column, table, IndexedColumn, indexedTable
    , Attribute, width, height, Length, px, shrink, fill, fillPortion, maximum, minimum
    , explain
    , padding, paddingXY, paddingEach
    , spacing, spacingXY, spaceEvenly
    , centerX, centerY, alignLeft, alignRight, alignTop, alignBottom
    , transparent, alpha, pointer
    , moveUp, moveDown, moveRight, moveLeft, rotate, scale
    , clip, clipX, clipY
    , scrollbars, scrollbarX, scrollbarY
    , layout, layoutWith, Option, noStaticStyleSheet, forceHover, noHover, focusStyle
    , link, newTabLink, download, downloadAs
    , image
    , Color, rgba, rgb, rgb255, rgba255, fromRgb, fromRgb255, toRgb
    , above, below, onRight, onLeft, inFront, behindContent
    , Attr, Decoration, mouseOver, mouseDown, focused
    , Device, DeviceClass(..), Orientation(..), classifyDevice
    , modular
    , html, htmlAttribute
    ) where

-- import Html exposing (Html)
-- import Html.Attributes

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.Props as P
import Data.Array ((:))
import Data.Array as Array
import Data.BooleanAlgebra ((&&))
import Data.CommutativeRing ((*))
import Data.Eq ((==))
import Data.EuclideanRing ((+), (-), (/))
import Data.Field (negate)
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isNothing)
import Data.Ord (max, min, (<), (<=), (>), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Data.Void (Void)
import Internal.Flag as Flag
import Internal.Model (FocusStyle, arrayFoldl, isElementEmpty)
import Internal.Model as Internal
import Internal.Style (classes)
import Internal.Style as IStyle
import Math (pow)
import Util (zeroDiv)


{-| -}
type Color =
    Internal.Color


{-| Provide the red, green, and blue channels for the color.

Each channel takes a value between 0 and 1.

-}
rgb :: Number -> Number -> Number -> Color
rgb r g b =
    Internal.Rgba r g b 1.0


{-| -}
rgba :: Number -> Number -> Number -> Number -> Color
rgba =
    Internal.Rgba


{-| Provide the red, green, and blue channels for the color.

Each channel takes a value between 0 and 255.

-}
rgb255 :: Int -> Int -> Int -> Color
rgb255 red green blue =
    Internal.Rgba
        (toNumber red / 255.0)
        (toNumber green / 255.0)
        (toNumber blue / 255.0)
        1.0


{-| -}
rgba255 :: Int -> Int -> Int -> Number -> Color
rgba255 red green blue a =
    Internal.Rgba
        (toNumber red / 255.0)
        (toNumber green / 255.0)
        (toNumber blue / 255.0)
        a


{-| Create a color from an RGB record.
-}
fromRgb ::
    { red :: Number
    , green :: Number
    , blue :: Number
    , alpha :: Number
    }
    -> Color
fromRgb clr =
    Internal.Rgba
        clr.red
        clr.green
        clr.blue
        clr.alpha


{-| -}
fromRgb255 ::
    { red :: Int
    , green :: Int
    , blue :: Int
    , alpha :: Number
    }
    -> Color
fromRgb255 clr =
    Internal.Rgba
        (toNumber clr.red / 255.0)
        (toNumber clr.green / 255.0)
        (toNumber clr.blue / 255.0)
        clr.alpha


{-| Deconstruct a `Color` into its rgb channels.
-}
toRgb ::
    Color
    ->
        { red :: Number
        , green :: Number
        , blue :: Number
        , alpha :: Number
        }
toRgb (Internal.Rgba r g b a) =
    { red: r
    , green: g
    , blue: b
    , alpha: a
    }


{-| The basic building block of your layout.

    howdy :: Element msg
    howdy =
        Element.el [] (Element.text "Howdy!")

-}
type Element msg =
    Internal.Element msg


{-| An attribute that can be attached to an `Element`
-}
type Attribute msg =
    Internal.Attribute Unit msg


{-| This is a special attribute that counts as both a `Attribute msg` and a `Decoration`.
-}
type Attr decorative msg =
    Internal.Attribute decorative msg


{-| Only decorations
-}
type Decoration =
    Internal.Attribute Void Void


{-| -}
html :: forall msg. Widget HTML msg -> Element msg
html =
    Internal.unstyled


{-| -}
htmlAttribute :: forall msg. P.ReactProps msg -> Attribute msg
htmlAttribute =
    Internal.Attr


{-| -}
type Length =
    Internal.Length


{-| -}
px :: Int -> Length
px =
    Internal.Px


{-| Shrink an element to fit its contents.
-}
shrink :: Length
shrink =
    Internal.Content


{-| Fill the available space. The available space will be split evenly between elements that have `width fill`.
-}
fill :: Length
fill =
    Internal.Fill 1


{-| Similarly you can set a minimum boundary.

     el
        [ height
            (fill
                # maximum 300
                # minimum 30
            )

        ]
        (text "I will stop at 300px")

-}
minimum :: Int -> Length -> Length
minimum i l =
    Internal.Min i l


{-| Add a maximum to a length.

    el
        [ height
            (fill
                # maximum 300
            )
        ]
        (text "I will stop at 300px")

-}
maximum :: Int -> Length -> Length
maximum i l =
    Internal.Max i l


{-| Sometimes you may not want to split available space evenly. In this case you can use `fillPortion` to define which elements should have what portion of the available space.

So, two elements, one with `width (fillPortion 2)` and one with `width (fillPortion 3)`. The first would get 2 portions of the available space, while the second would get 3.

**Also:** `fill == fillPortion 1`

-}
fillPortion :: Int -> Length
fillPortion =
    Internal.Fill


{-| This is your top level node where you can turn `Element` into `Html`.
-}
layout :: forall msg. Array (Attribute msg) -> Element msg -> Widget HTML msg
layout =
    layoutWith { options: [] }


{-| -}
layoutWith :: forall msg. { options :: Array Option } -> Array (Attribute msg) -> Element msg -> Widget HTML msg
layoutWith { options } attrs child =
    Internal.renderRoot options
        (Internal.htmlClass
            (String.joinWith " "
                [ classes.root
                , classes.any
                , classes.single
                ]
            ) : (Internal.rootStyle <> attrs)
        )
        child


{-| -}
type Option =
    Internal.Option


{-| Elm UI embeds two StyleSheets, one that is constant, and one that changes dynamically based on styles collected from the elements being rendered.

This option will stop the static/constant stylesheet from rendering.

If you're embedding multiple elm-ui `layout` elements, you need to guarantee that only one is rendering the static style sheet and that it's above all the others in the DOM tree.

-}
noStaticStyleSheet :: Option
noStaticStyleSheet =
    Internal.RenderModeOption Internal.NoStaticStyleSheet

{-| -}
defaultFocus :: FocusStyle
defaultFocus =
    Internal.focusDefaultStyle


{-| -}
focusStyle :: FocusStyle -> Option
focusStyle =
    Internal.FocusStyleOption


{-| Disable all `mouseOver` styles.
-}
noHover :: Option
noHover =
    Internal.HoverOption Internal.NoHover


{-| Any `hover` styles, aka attributes with `mouseOver` in the name, will be always turned on.

This is useful for when you're targeting a platform that has no mouse, such as mobile.

-}
forceHover :: Option
forceHover =
    Internal.HoverOption Internal.ForceHover


{-| When you want to render exactly nothing.
-}
none :: forall msg. Element msg
none =
    Internal.Empty


{-| Create some plain text.

    text "Hello, you stylish developer!"

**Note** text does not wrap by default. In order to get text to wrap, check out `paragraph`!

-}
text :: forall msg. String -> Element msg
text content =
    Internal.Text content


{-| The basic building block of your layout.

You can think of an `el` as a `div`, but it can only have one child.

If you want multiple children, you'll need to use something like `row` or `column`

    import Element exposing (Element, rgb)
    import Element.Background as Background
    import Element.Border as Border

    myElement :: Element msg
    myElement =
        Element.el
            [ Background.color (rgb 0 0.5 0)
            , Border.color (rgb 0 0.7 0)
            ]
            (Element.text "You've made a stylish element!")

-}
el :: forall msg. Array (Attribute msg) -> Element msg -> Element msg
el attrs child =
    Internal.element
        Internal.asEl
        Internal.div
        (width shrink
            : height shrink
            : attrs
        )
        (Internal.Unkeyed [ child ])


{-| -}
row :: forall msg. Array (Attribute msg) -> Array (Element msg) -> Element msg
row attrs children =
    Internal.element
        Internal.asRow
        Internal.div
        (Internal.htmlClass (classes.contentLeft <> " " <> classes.contentCenterY)
            : width shrink
            : height shrink
            : attrs
        )
        (Internal.Unkeyed children)


{-| -}
column :: forall msg. Array (Attribute msg) -> Array (Element msg) -> Element msg
column attrs children =
    Internal.element
        Internal.asColumn
        Internal.div
        (Internal.htmlClass
            (classes.contentTop
                <> " "
                <> classes.contentLeft
            )
            : height shrink
            : width shrink
            : attrs
        )
        (Internal.Unkeyed children)


{-| Same as `row`, but will wrap if it takes up too much horizontal space.
-}
wrappedRow :: forall msg. Array (Attribute msg) -> Array (Element msg) -> Element msg
wrappedRow attrs children =
    let
        ( Tuple padded spaced ) =
            Internal.extractSpacingAndPadding attrs
    in
    case spaced of
        Nothing ->
            Internal.element
                Internal.asRow
                Internal.div
                (Internal.htmlClass
                    (classes.contentLeft
                        <> " "
                        <> classes.contentCenterY
                        <> " "
                        <> classes.wrapped
                    )
                    : width shrink
                    : height shrink
                    : attrs
                )
                (Internal.Unkeyed children)

        Just (Internal.Spaced spaceName x y) ->
            let
                newPadding =
                    case padded of
                        Just (Internal.Padding name t r b l) ->
                            if r >= (x `zeroDiv` 2) && b >= (y `zeroDiv` 2) then
                                Just $
                                    paddingEach
                                        { top: t - (y `zeroDiv` 2)
                                        , right: r - (x `zeroDiv` 2)
                                        , bottom: b - (y `zeroDiv` 2)
                                        , left: l - (x `zeroDiv` 2)
                                        }

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            case newPadding of
                Just pad ->
                    Internal.element
                        Internal.asRow
                        Internal.div
                        (Internal.htmlClass
                            (classes.contentLeft
                                <> " "
                                <> classes.contentCenterY
                                <> " "
                                <> classes.wrapped
                            )
                            : width shrink
                            : height shrink
                            : attrs
                            <> [ pad ]
                        )
                        (Internal.Unkeyed children)

                Nothing ->
                    -- Not enough space in padding to compensate for spacing
                    let
                        halfX =
                            negate (toNumber x / 2.0)

                        halfY =
                            negate (toNumber y / 2.0)
                    in
                    Internal.element
                        Internal.asEl
                        Internal.div
                        attrs
                        (Internal.Unkeyed
                            [ Internal.element
                                Internal.asRow
                                Internal.div
                                (Internal.htmlClass
                                    (classes.contentLeft
                                        <> " "
                                        <> classes.contentCenterY
                                        <> " "
                                        <> classes.wrapped
                                    )
                                    : Internal.Attr
                                        (P.style
                                          { margin:
                                            (show halfY -- Number
                                                <> "px"
                                                <> " "
                                                <> show halfX -- Number
                                                <> "px"
                                            )
                                          , width:
                                            ("calc(100% + "
                                                <> show x -- Int
                                                <> "px)"
                                            )
                                          , height:
                                            ("calc(100% + "
                                                <> show y -- Int
                                                <> "px)"
                                            )
                                          }
                                        )
                                    : Internal.StyleClass Flag.spacing (Internal.SpacingStyle spaceName x y)
                                    : []
                                )
                                (Internal.Unkeyed children)
                            ]
                        )


{-| This is just an alias for `Debug.todo`
-}
type Todo =
    String -> Void


{-| Highlight the borders of an element and it's children below. This can really help if you're running into some issue with your layout!

**Note** This attribute needs to be handed `Debug.todo` in order to work, even though it won't do anything with it. This is a safety measure so you don't accidently ship code with `explain` in it, as Elm won't compile with `--optimize` if you still have a `Debug` statement in your code.

    el
        [ Element.explain Debug.todo
        ]
        (text "Help, I'm being debugged!")

-}
explain :: forall msg. Todo -> Attribute msg
explain _ =
    Internal.htmlClass "explain"


{-| -}
type Column record msg =
    { header :: Element msg
    , width :: Length
    , view :: record -> Element msg
    }


{-| Show some tabular data.

Start with a list of records and specify how each column should be rendered.

So, if we have a list of `persons`:

    type Person =
        { firstName :: String
        , lastName :: String
        }

    persons :: Array Person
    persons =
        [ { firstName = "David"
          , lastName = "Bowie"
          }
        , { firstName = "Florence"
          , lastName = "Welch"
          }
        ]

We could render it using

    Element.table []
        { data = persons
        , columns =
            [ { header = Element.text "First Name"
              , width = fill
              , view =
                    \person ->
                        Element.text person.firstName
              }
            , { header = Element.text "Last Name"
              , width = fill
              , view =
                    \person ->
                        Element.text person.lastName
              }
            ]
        }

**Note:** Sometimes you might not have a list of records directly in your model. In this case it can be really nice to write a function that transforms some part of your model into a list of records before feeding it into `Element.table`.

-}
table ::
    forall msg records. Array (Attribute msg)
    ->
        { data :: Array records
        , columns :: Array (Column records msg)
        }
    -> Element msg
table attrs config =
    tableHelper attrs
        { data: config.data
        , columns:
            map InternalColumn config.columns
        }


{-| -}
type IndexedColumn record msg =
    { header :: Element msg
    , width :: Length
    , view :: Int -> record -> Element msg
    }


{-| Same as `Element.table` except the `view` for each column will also receive the row index as well as the record.
-}
indexedTable ::
    forall msg records. Array (Attribute msg)
    ->
        { data :: Array records
        , columns :: Array (IndexedColumn records msg)
        }
    -> Element msg
indexedTable attrs config =
    tableHelper attrs
        { data: config.data
        , columns:
            map InternalIndexedColumn config.columns
        }


{-| -}
type InternalTable records msg =
    { data :: Array records
    , columns :: Array (InternalTableColumn records msg)
    }


{-| -}
data InternalTableColumn record msg
    = InternalIndexedColumn (IndexedColumn record msg)
    | InternalColumn (Column record msg)


tableHelper :: forall msg dat. Array (Attribute msg) -> InternalTable dat msg -> Element msg
tableHelper attrs config =
    let
        ( Tuple sX sY ) =
            Internal.getSpacing attrs ( Tuple 0 0 )

        columnHeader col =
            case col of
                InternalIndexedColumn colConfig ->
                    colConfig.header

                InternalColumn colConfig ->
                    colConfig.header

        columnWidth col =
            case col of
                InternalIndexedColumn colConfig ->
                    colConfig.width

                InternalColumn colConfig ->
                    colConfig.width

        maybeHeaders =
            map columnHeader config.columns
                # (\headers ->
                        if Array.all isElementEmpty headers then
                            Nothing

                        else
                            Just (Array.mapWithIndex (\col header -> onGrid 1 (col + 1) header) headers)
                   )

        template =
            Internal.StyleClass Flag.gridTemplate $
                Internal.GridTemplateStyle
                    { spacing: { xval: px sX, yval: px sY }
                    , columns: map columnWidth config.columns
                    , rows: Array.replicate (Array.length config.data) Internal.Content
                    }

        onGrid rowLevel columnLevel elem =
            Internal.element
                Internal.asEl
                Internal.div
                [ Internal.StyleClass Flag.gridPosition
                    (Internal.GridPosition
                        { row: rowLevel
                        , col: columnLevel
                        , width: 1
                        , height: 1
                        }
                    )
                ]
                (Internal.Unkeyed [ elem ])

        add cell columnConfig cursor =
            case columnConfig of
                InternalIndexedColumn col ->
                  cursor
                    { elements =
                        (onGrid cursor.row
                            cursor.column
                            ((col.view
                                (if isNothing maybeHeaders then
                                    cursor.row - 1

                                 else
                                    cursor.row - 2
                                )
                                cell
                              )) :
                              cursor.elements)
                    , column = cursor.column + 1
                    }

                InternalColumn col ->
                    { elements:
                        Array.cons (onGrid cursor.row cursor.column (col.view cell)) cursor.elements
                    , column: cursor.column + 1
                    , row: cursor.row
                    }

        build columns rowData cursor =
            let
                newCursor =
                    arrayFoldl (add rowData)
                        cursor
                        columns
            in
            { elements: newCursor.elements
            , row: cursor.row + 1
            , column: 1
            }

        children =
            arrayFoldl (build config.columns)
                { elements: []
                , row:
                    if isNothing maybeHeaders then
                        1

                    else
                        2
                , column: 1
                }
                config.data
    in
    Internal.element
        Internal.asGrid
        Internal.div
        (Array.cons (width fill) (Array.cons template attrs))
        (Internal.Unkeyed
            (case maybeHeaders of
                Nothing ->
                    children.elements

                Just renderedHeaders ->
                    renderedHeaders <> Array.reverse children.elements
            )
        )


{-| A paragraph will layout all children as wrapped, inline elements.

    import Element
    import Element.Font as Font

    Element.paragraph []
        [ text "lots of text ...."
        , el [ Font.bold ] (text "this is bold")
        , text "lots of text ...."
        ]

This is really useful when you want to markup text by having some parts be bold, or some be links, or whatever you so desire.

Also, if a child element has `alignLeft` or `alignRight`, then it will be moved to that side and the text will flow around it, (ah yes, `float` behavior).

This makes it particularly easy to do something like a [dropped capital](https://en.wikipedia.org/wiki/Initial).

    import Element
    import Element.Font as Font

    Element.paragraph []
        [ el
            [ alignLeft
            , padding 5
            ]
            (text "S")
        , text "o much text ...."
        ]

Which will look something like

![A paragraph where the first letter is twice the height of the others](https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%209.41.52%20PM.png)

**Note** `spacing` on a paragraph will set the pixel spacing between lines.

-}
paragraph :: forall msg. Array (Attribute msg) -> Array (Element msg) -> Element msg
paragraph attrs children =
    Internal.element
        Internal.asParagraph
        Internal.div
        (Array.cons (Internal.Describe Internal.Paragraph)
            (Array.cons (width fill)
              (Array.cons (spacing 5) attrs)))
        (Internal.Unkeyed children)


{-| Now that we have a paragraph, we need some way to attach a bunch of paragraph's together.

To do that we can use a `textColumn`.

The main difference between a `column` and a `textColumn` is that `textColumn` will flow the text around elements that have `alignRight` or `alignLeft`, just like we just saw with paragraph.

In the following example, we have a `textColumn` where one child has `alignLeft`.

    Element.textColumn [ spacing 10, padding 10 ]
        [ paragraph [] [ text "lots of text ...." ]
        , el [ alignLeft ] none
        , paragraph [] [ text "lots of text ...." ]
        ]

Which will result in something like:

![A text layout where an image is on the left.](https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%208.42.39%20PM.png)

-}
textColumn :: forall msg. Array (Attribute msg) -> Array (Element msg) -> Element msg
textColumn attrs children =
    Internal.element
        Internal.asTextColumn
        Internal.div
        (width
            (fill
                # minimum 500
                # maximum 750
            )
            : attrs
        )
        (Internal.Unkeyed children)


{-| Both a source and a description are required for images.

The description is used for people using screen readers.

Leaving the description blank will cause the image to be ignored by assistive technology. This can make sense for images that are purely decorative and add no additional information.

So, take a moment to describe your image as you would to someone who has a harder time seeing.

-}
image :: forall msg. Array (Attribute msg) -> { src :: String, description :: String } -> Element msg
image attrs { src, description } =
    let
        imageAttributes =
            attrs
                # Array.filter
                    (\a ->
                        case a of
                            Internal.Width _ ->
                                true

                            Internal.Height _ ->
                                true

                            _ ->
                                false
                    )
    in
    Internal.element
        Internal.asEl
        Internal.div
        (Internal.htmlClass classes.imageContainer
            : attrs
        )
        (Internal.Unkeyed
            [ Internal.element
                Internal.asEl
                (Internal.NodeName "img")
                ([ Internal.Attr $ P.src src
                 , Internal.Attr $ P.alt description
                 ]
                    <> imageAttributes
                )
                (Internal.Unkeyed [])
            ]
        )


{-|

    link []
        { url = "http://fruits.com"
        , label = text "A link to my favorite fruit provider."
        }

-}
link :: forall msg. Array (Attribute msg)
    -> { url :: String
       , label :: Element msg
       }
    -> Element msg
link attrs { url, label } =
    Internal.element
        Internal.asEl
        (Internal.NodeName "a")
        (Internal.Attr (P.href url)
            : Internal.Attr (P.rel "noopener noreferrer")
            : width shrink
            : height shrink
            : Internal.htmlClass
                (classes.contentCenterX
                    <> " "
                    <> classes.contentCenterY
                    <> " "
                    <> classes.link
                )
            : attrs
        )
        (Internal.Unkeyed [ label ])


{-| -}
newTabLink :: forall msg.
    Array (Attribute msg)
    ->
        { url :: String
        , label :: Element msg
        }
    -> Element msg
newTabLink attrs { url, label } =
    Internal.element
        Internal.asEl
        (Internal.NodeName "a")
        (Internal.Attr (P.href url)
            : Internal.Attr (P.rel "noopener noreferrer")
            : Internal.Attr (P.target "_blank")
            : width shrink
            : height shrink
            : Internal.htmlClass
                (classes.contentCenterX
                    <> " "
                    <> classes.contentCenterY
                    <> " "
                    <> classes.link
                )
            : attrs
        )
        (Internal.Unkeyed [ label ])


{-| A link to download a file.

**Note** If you're using `Browser.application`, then this won't be enough to actually trigger a file download due to how `Browser.Navigation` works.

[Here's a description of what needs to happen](https://github.com/elm/html/issues/175).

-}
download ::
    forall msg. Array (Attribute msg)
    ->
        { url :: String
        , label :: Element msg
        }
    -> Element msg
download attrs { url, label } =
    Internal.element
        Internal.asEl
        (Internal.NodeName "a")
        (Internal.Attr (P.href url)
            : Internal.Attr (P.download "")
            : width shrink
            : height shrink
            : Internal.htmlClass classes.contentCenterX
            : Internal.htmlClass classes.contentCenterY
            : attrs
        )
        (Internal.Unkeyed [ label ])


{-| A link to download a file, but you can specify the filename.
-}
downloadAs ::
    forall msg. Array (Attribute msg)
    ->
        { label :: Element msg
        , filename :: String
        , url :: String
        }
    -> Element msg
downloadAs attrs { url, filename, label } =
    Internal.element
        Internal.asEl
        (Internal.NodeName "a")
        (Internal.Attr (P.href url)
            : Internal.Attr (P.download filename)
            : width shrink
            : height shrink
            : Internal.htmlClass classes.contentCenterX
            : Internal.htmlClass classes.contentCenterY
            : attrs
        )
        (Internal.Unkeyed [ label ])



{- NEARBYS -}


createNearby :: forall msg. Internal.Location -> Element msg -> Attribute msg
createNearby loc element =
    case element of
        Internal.Empty ->
            Internal.NoAttribute

        _ ->
            Internal.Nearby loc element


{-| -}
below :: forall msg. Element msg -> Attribute msg
below element =
    createNearby Internal.Below element


{-| -}
above :: forall msg. Element msg -> Attribute msg
above element =
    createNearby Internal.Above element


{-| -}
onRight :: forall msg. Element msg -> Attribute msg
onRight element =
    createNearby Internal.OnRight element


{-| -}
onLeft :: forall msg. Element msg -> Attribute msg
onLeft element =
    createNearby Internal.OnLeft element


{-| This will place an element in front of another.

**Note:** If you use this on a `layout` element, it will place the element as fixed to the viewport which can be useful for modals and overlays.

-}
inFront :: forall msg. Element msg -> Attribute msg
inFront element =
    createNearby Internal.InFront element


{-| This will place an element between the background and the content of an element.
-}
behindContent :: forall msg. Element msg -> Attribute msg
behindContent element =
    createNearby Internal.Behind element


{-| -}
width :: forall msg. Length -> Attribute msg
width =
    Internal.Width


{-| -}
height :: forall msg. Length -> Attribute msg
height =
    Internal.Height


{-| -}
scale :: forall decorative msg. Number -> Attr decorative msg
scale n =
    Internal.TransformComponent Flag.scale (Internal.Scale {x:n, y:n, z:1.0})


{-| Angle is given in radians. [Here are some conversion functions if you want to use another unit.](https://package.elm-lang.org/packages/elm/core/latest/Basics#degrees)
-}
rotate :: forall decorative msg. Number -> Attr decorative msg
rotate angle =
    Internal.TransformComponent Flag.rotate (Internal.Rotate {x:0.0, y:0.0, z:1.0} angle)


{-| -}
moveUp :: forall decorative msg. Number -> Attr decorative msg
moveUp y =
    Internal.TransformComponent Flag.moveY (Internal.MoveY (negate y))


{-| -}
moveDown :: forall decorative msg. Number -> Attr decorative msg
moveDown y =
    Internal.TransformComponent Flag.moveY (Internal.MoveY y)


{-| -}
moveRight :: forall decorative msg. Number -> Attr decorative msg
moveRight x =
    Internal.TransformComponent Flag.moveX (Internal.MoveX x)


{-| -}
moveLeft :: forall decorative msg. Number -> Attr decorative msg
moveLeft x =
    Internal.TransformComponent Flag.moveX (Internal.MoveX (negate x))


{-| -}
padding :: forall msg. Int -> Attribute msg
padding x =
    Internal.StyleClass Flag.padding (Internal.PaddingStyle ("p-" <> show x) x x x x) -- Int


{-| Set horizontal and vertical padding.
-}
paddingXY :: forall msg. Int -> Int -> Attribute msg
paddingXY x y =
    if x == y then
        Internal.StyleClass Flag.padding (Internal.PaddingStyle ("p-" <> show x) x x x x) -- Int

    else
        Internal.StyleClass Flag.padding
            (Internal.PaddingStyle
                ("p-" <> show x <> "-" <> show y) -- Int
                y
                x
                y
                x
            )


{-| If you find yourself defining unique paddings all the time, you might consider defining

    edges =
        { top = 0
        , right = 0
        , bottom = 0
        , left = 0
        }

And then just do

    paddingEach { edges | right = 5 }

-}
paddingEach :: forall msg. { top :: Int, right :: Int, bottom :: Int, left :: Int } -> Attribute msg
paddingEach { top, right, bottom, left } =
    if top == right && top == bottom && top == left then
        Internal.StyleClass Flag.padding (Internal.PaddingStyle ("p-" <> show top) top top top top) -- Int

    else
        Internal.StyleClass Flag.padding
            (Internal.PaddingStyle
                (Internal.paddingName top right bottom left)
                top
                right
                bottom
                left
            )


{-| -}
centerX :: forall msg. Attribute msg
centerX =
    Internal.AlignX Internal.CenterX


{-| -}
centerY :: forall msg. Attribute msg
centerY =
    Internal.AlignY Internal.CenterY


{-| -}
alignTop :: forall msg. Attribute msg
alignTop =
    Internal.AlignY Internal.Top


{-| -}
alignBottom :: forall msg. Attribute msg
alignBottom =
    Internal.AlignY Internal.Bottom


{-| -}
alignLeft :: forall msg. Attribute msg
alignLeft =
    Internal.AlignX Internal.Left


{-| -}
alignRight :: forall msg. Attribute msg
alignRight =
    Internal.AlignX Internal.Right


{-| -}
spaceEvenly :: forall msg. Attribute msg
spaceEvenly =
    Internal.Class Flag.spacing IStyle.classes.spaceEvenly


{-| -}
spacing :: forall msg. Int -> Attribute msg
spacing x =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle (Internal.spacingName x x) x x)


{-| In the majority of cases you'll just need to use `spacing`, which will work as intended.

However for some layouts, like `textColumn`, you may want to set a different spacing for the x axis compared to the y axis.

-}
spacingXY :: forall msg. Int -> Int -> Attribute msg
spacingXY x y =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle (Internal.spacingName x y) x y)


{-| Make an element transparent and have it ignore any mouse or touch events, though it will stil take up space.
-}
transparent :: forall decorative msg. Boolean -> Attr decorative msg
transparent on =
    if on then
        Internal.StyleClass Flag.transparency (Internal.Transparency "transparent" 1.0)

    else
        Internal.StyleClass Flag.transparency (Internal.Transparency "visible" 0.0)


{-| A capped value between 0.0 and 1.0, where 0.0 is transparent and 1.0 is fully opaque.

Semantically equivalent to html opacity.

-}
alpha :: forall msg decorative. Number -> Attr decorative msg
alpha o =
    let
        transparency =
            o
                # max 0.0
                # min 1.0
                # (\x -> 1.0 - x)
    in
    Internal.StyleClass Flag.transparency $ Internal.Transparency ("transparency-" <> Internal.floatClass transparency) transparency



-- {-| -}
-- hidden :: Boolean -> Attribute msg
-- hidden on =
--     if on then
--         Internal.class "hidden"
--     else
--         Internal.NoAttribute


{-| -}
scrollbars :: forall msg. Attribute msg
scrollbars =
    Internal.Class Flag.overflow classes.scrollbars


{-| -}
scrollbarY :: forall msg. Attribute msg
scrollbarY =
    Internal.Class Flag.overflow classes.scrollbarsY


{-| -}
scrollbarX :: forall msg. Attribute msg
scrollbarX =
    Internal.Class Flag.overflow classes.scrollbarsX


{-| -}
clip :: forall msg. Attribute msg
clip =
    Internal.Class Flag.overflow classes.clip


{-| -}
clipY :: forall msg. Attribute msg
clipY =
    Internal.Class Flag.overflow classes.clipY


{-| -}
clipX :: forall msg. Attribute msg
clipX =
    Internal.Class Flag.overflow classes.clipX


{-| Set the cursor to be a pointing hand when it's hovering over this element.
-}
pointer :: forall msg. Attribute msg
pointer =
    Internal.Class Flag.cursor classes.cursorPointer


{-| -}
type Device =
    { class :: DeviceClass
    , orientation :: Orientation
    }


{-| -}
data DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


{-| -}
data Orientation
    = Portrait
    | Landscape


{-| Takes in a Window.Size and returns a device profile which can be used for responsiveness.

If you have more detailed concerns around responsiveness, it probably makes sense to copy this function into your codebase and modify as needed.

-}
classifyDevice :: {height :: Int, width :: Int } -> Device
classifyDevice window =
    -- Tested in this ellie:
    -- https://ellie-app.com/68QM7wLW8b9a1
    { class:
        let
            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if shortSide < 600 then
            Phone

        else if longSide <= 1200 then
            Tablet

        else if longSide > 1200 && longSide <= 1920 then
            Desktop

        else
            BigDesktop
    , orientation:
        if window.width < window.height then
            Portrait

        else
            Landscape
    }


{-| When designing it's nice to use a modular scale to set spacial rythms.

    scaled =
        Element.modular 16 1.25

A modular scale starts with a number, and multiplies it by a ratio a number of times.
Then, when setting font sizes you can use:

    Font.size (scaled 1) -- results in 16

    Font.size (scaled 2) -- 16 * 1.25 results in 20

    Font.size (scaled 4) -- 16 * 1.25 ^ (4 - 1) results in 31.25

We can also provide negative numbers to scale below 16px.

    Font.size (scaled -1) -- 16 * 1.25 ^ (-1) results in 12.8

-}
modular :: Number -> Number -> Int -> Number
modular normal ratio rescale =
    if rescale == 0 then
        normal

    else if rescale < 0 then
        normal * ratio `pow` toNumber rescale

    else
        normal * ratio `pow` (toNumber rescale - 1.0)


{-| -}
mouseOver :: forall msg. Array Decoration -> Attribute msg
mouseOver decs =
    Internal.StyleClass Flag.hover $
        Internal.PseudoSelector Internal.Hover
            (Internal.unwrapDecorations decs)


{-| -}
mouseDown :: forall msg. Array Decoration -> Attribute msg
mouseDown decs =
    Internal.StyleClass Flag.active $
        Internal.PseudoSelector Internal.Active
            (Internal.unwrapDecorations decs)


{-| -}
focused :: forall msg. Array Decoration -> Attribute msg
focused decs =
    Internal.StyleClass Flag.focus $
        Internal.PseudoSelector Internal.Focus
            (Internal.unwrapDecorations decs)
