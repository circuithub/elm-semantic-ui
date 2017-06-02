module SemanticUI.Elements.Image
    exposing
        ( Config
        , init
        , image
        , centered
        , size
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

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


{-| All properties of an image.
-}
type alias Config =
    { src : String
    , size : Maybe Size
    , centered : Bool
    }


{-| The initial config of an image. Corresponds to just using `class="ui image"` in Semantic UI.
-}
init : Config
init =
    { src = ""
    , size = Nothing
    , centered = False
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


{-| View an `<img>` element with a particular `src` (the url of the image to display).
-}
image : Config -> String -> Html msg
image { size, centered } src =
    img
        (List.concat
            [ [ Html.Attributes.src src
              , class "ui image"
              , classList
                    [ ( "centered", centered )
                    ]
              ]
            , case size of
                Just size ->
                    [ sizeClass size ]

                Nothing ->
                    []
            ]
        )
        []
