module SemanticUI.Elements.Image exposing
    ( image
    , init, Config
    , bordered, rounded, spaced
    , size
    , centered
    , inline, VerticalAlignment(..)
    )

{-| An image is a graphic representation of something.


# Viewing images

@docs image


# Image properties

@docs init, Config
@docs bordered, rounded, spaced


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
    , rounded : Bool
    , bordered : Bool
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
    , rounded = False
    , bordered = False
    }


{-| Specify whether or not an image should display at a particular size.
-}
size : Maybe Size -> Config -> Config
size a model =
    { model | size = a }


{-| Specify whether or not an image should appear centered.
-}
centered : Bool -> Config -> Config
centered a model =
    { model | centered = a }


{-| Specify whether or not an image should appear inline, and how it should be
vertically aligned.
-}
inline : Maybe VerticalAlignment -> Config -> Config
inline a model =
    { model | inline = a }


{-| Whether to add space around the image to separate it from its surroundings.
-}
spaced : Bool -> Config -> Config
spaced a model =
    { model | spaced = a }


{-| Whether to round the corners of the image.
-}
rounded : Bool -> Config -> Config
rounded a model =
    { model | rounded = a }


{-| Whether to add a border around the image.
-}
bordered : Bool -> Config -> Config
bordered a model =
    { model | bordered = a }


{-| View an `<img>` element with a particular `src` (the url of the image to display).
-}
image : Config -> String -> Html msg
image cfg src =
    img
        (List.concat
            [ [ Html.Attributes.src src
              , class "ui image"
              , classList
                    [ ( "centered", cfg.centered )
                    , ( "spaced", cfg.spaced )
                    , ( "rounded", cfg.rounded )
                    , ( "bordered", cfg.bordered )
                    ]
              ]
            , case cfg.size of
                Just a ->
                    [ sizeClass a ]

                Nothing ->
                    []
            , case cfg.inline of
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
