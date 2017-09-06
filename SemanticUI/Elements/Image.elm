module SemanticUI.Elements.Image
    exposing
        ( Config
        , init
        , image
        , centered
        , size
        , inline
        , spaced
        , VerticalAlignment(..)
        )

{-| An image is a graphic representation of something.

# Viewing images

@docs image

# Image properties

@docs init, Config

## Size

An image may appear at different sizes.

@docs size

## Centered

An image can appear centered in a content block.

@docs centered

## Inline

An image can appear inline and be vertically centered.

@docs inline, VerticalAlignment

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


{-| How an image is vertically aligned when displayed inline.
-}
type VerticalAlignment
    = TopAligned
    | MiddleAligned
    | BottomAligned


{-| All properties of an image.
-}
type alias Config =
    { src : String
    , size : Maybe Size
    , centered : Bool
    , inline : Maybe VerticalAlignment
    , spaced : Bool
    }


{-| The initial config of an image. Corresponds to just using `class="ui image"` in Semantic UI.
-}
init : Config
init =
    { src = ""
    , size = Nothing
    , centered = False
    , inline = Nothing
    , spaced = False
    }


{-| Specify whether or not an image should display at a particular size.
-}
size : Maybe Size -> Config -> Config
size size model =
    { model | size = size }


{-| Specify whether or not an image should appear centered.
-}
centered : Bool -> Config -> Config
centered centered model =
    { model | centered = centered }


{-| Specify whether or not an image should appear inline, and how it should be
vertically aligned.
-}
inline : Maybe VerticalAlignment -> Config -> Config
inline inline model =
    { model | inline = inline }


spaced : Bool -> Config -> Config
spaced spaced model =
    { model | spaced = spaced }


{-| View an `<img>` element with a particular `src` (the url of the image to display).
-}
image : Config -> String -> Html msg
image { size, centered, inline, spaced } src =
    img
        (List.concat
            [ [ Html.Attributes.src src
              , class "ui image"
              , classList
                    [ ( "centered", centered )
                    , ( "spaced", spaced )
                    ]
              ]
            , case size of
                Just size ->
                    [ sizeClass size ]

                Nothing ->
                    []
            , case inline of
                Nothing ->
                    []

                Just v ->
                    [ class <|
                        case v of
                            TopAligned ->
                                "top aligned"

                            MiddleAligned ->
                                "middle aligned"

                            BottomAligned ->
                                "bottom aligned"
                    ]
            ]
        )
        []
