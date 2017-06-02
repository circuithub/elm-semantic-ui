module SemanticUI.Elements.Label
    exposing
        ( init
        , Config
        , label
        , size
        , pointing
        , Pointing
        , image
        , detail
        , color
        , circular
        , basic
        )

{-|
A label displays content classification.

# Viewing labels

@docs label

# Label properties

@docs init, Config

## Image

A label can be formatted to emphasize an image.

@docs image

## Color

A label can have different colors.

@docs color

## Detail

A label can contain extra detail.

@docs detail

## Pointing

A label can point to content next to it.

@docs pointing, Pointing

## Basic

A label can reduce its complexity.

@docs basic

## Tag

A label can appear as a tag.

## Horizontal

A horizontal label is formatted to label content along-side it horizontally.

## Circular

A label can be circular.

@docs circular

## Size

A label can be small or large.

@docs size
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


{-| The configuration of a label.
-}
type alias Config =
    { image : Maybe String
    , color : Maybe Color
    , detail : Maybe String
    , pointing : Maybe Pointing
    , basic : Bool
    , tag : Bool
    , horizontal : Bool
    , circular : Bool
    , size : Size
    }


{-| Which direction a label can point.
-}
type Pointing
    = PointAbove
    | PointBelow
    | PointLeft
    | PointRight


{-| Specify the size of a label.
-}
size : Size -> Config -> Config
size size model =
    { model | size = size }


{-| Specify whether or not a label is circular.
-}
circular : Bool -> Config -> Config
circular circular model =
    { model | circular = circular }


tag : Bool -> Config -> Config
tag tag model =
    { model | tag = tag }


{-| Specify whether or not a label is basic.
-}
basic : Bool -> Config -> Config
basic basic model =
    { model | basic = basic }


{-| Specify whether a label should point to surrounding content.
-}
pointing : Maybe Pointing -> Config -> Config
pointing pointing model =
    { model | pointing = pointing }


{-| Specify the colour of a label.
-}
color : Maybe Color -> Config -> Config
color color model =
    { model | color = color }


{-| Specify extra detail to display in this label.
-}
detail : Maybe String -> Config -> Config
detail detail model =
    { model | detail = detail }


{-| Specify the URL to an image to display in this label.
-}
image : Maybe String -> Config -> Config
image image model =
    { model | image = image }


horizontal : Bool -> Config -> Config
horizontal horizontal model =
    { model | horizontal = horizontal }


{-| A label with the simplest configuration. Corresponds to just `class="ui label"`.
-}
init : Config
init =
    { image = Nothing
    , color = Nothing
    , detail = Nothing
    , pointing = Nothing
    , basic = False
    , tag = False
    , horizontal = False
    , circular = False
    , size = Medium
    }


{-| View a label with a specific configuration.
-}
label : Config -> String -> Html msg
label { image, color, detail, pointing, basic, tag, circular, horizontal, size } message =
    let
        content =
            [ text message ]
    in
        div
            (List.concat
                [ [ class "ui label"
                  , classList
                        [ ( "image", image /= Nothing )
                        , ( "basic", basic )
                        , ( "tag", tag )
                        , ( "horizontal", horizontal )
                        , ( "circular", circular )
                        ]
                  , sizeClass size
                  ]
                , case pointing of
                    Just PointAbove ->
                        [ class "pointing" ]

                    Just PointBelow ->
                        [ class "pointing below" ]

                    Just PointLeft ->
                        [ class "left pointing" ]

                    Just PointRight ->
                        [ class "right pointing" ]

                    Nothing ->
                        []
                , case color of
                    Just color ->
                        [ class <|
                            case color of
                                Blue ->
                                    "blue"

                                Teal ->
                                    "teal"

                                Yellow ->
                                    "yellow"

                                Red ->
                                    "red"
                        ]

                    Nothing ->
                        []
                ]
            )
        <|
            List.concat
                [ case image of
                    Just url ->
                        [ img [ src url ] [] ]

                    Nothing ->
                        []
                , content
                , case detail of
                    Just detail ->
                        [ div [ class "detail" ] [ Html.text detail ] ]

                    _ ->
                        []
                ]
