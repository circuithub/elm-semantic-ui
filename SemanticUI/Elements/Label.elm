module SemanticUI.Elements.Label
    exposing
        ( Config
        , Pointing
        , basic
        , circular
        , color
        , detail
        , horizontal
        , icon
        , image
        , init
        , label
        , link
        , pointing
        , size
        )

{-| A label displays content classification.


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
import SemanticUI.Elements.Icon as Icon


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
    , icon : Maybe Icon.Icon
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
size a model =
    { model | size = a }


{-| Specify whether or not a label is circular.
-}
circular : Bool -> Config -> Config
circular a model =
    { model | circular = a }


tag : Bool -> Config -> Config
tag a model =
    { model | tag = a }


{-| Specify whether or not a label is basic.
-}
basic : Bool -> Config -> Config
basic a model =
    { model | basic = a }


{-| Specify whether a label should point to surrounding content.
-}
pointing : Maybe Pointing -> Config -> Config
pointing a model =
    { model | pointing = a }


{-| Specify the colour of a label.
-}
color : Maybe Color -> Config -> Config
color a model =
    { model | color = a }


{-| Specify extra detail to display in this label.
-}
detail : Maybe String -> Config -> Config
detail a model =
    { model | detail = a }


{-| Specify the URL to an image to display in this label.
-}
image : Maybe String -> Config -> Config
image a model =
    { model | image = a }


horizontal : Bool -> Config -> Config
horizontal a model =
    { model | horizontal = a }


icon : Maybe Icon.Icon -> Config -> Config
icon a model =
    { model | icon = a }


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
    , icon = Nothing
    }


link : Config -> String -> String -> Html msg
link cfg url =
    viewAs (\attrs -> a (href url :: attrs)) cfg


label : Config -> String -> Html msg
label =
    viewAs div


{-| View a label with a specific configuration.
-}
viewAs el cfg message =
    let
        content =
            [ text message ]
    in
    el
        (List.concat
            [ [ class "ui label"
              , classList
                    [ ( "image", cfg.image /= Nothing )
                    , ( "basic", cfg.basic )
                    , ( "tag", cfg.tag )
                    , ( "horizontal", cfg.horizontal )
                    , ( "circular", cfg.circular )
                    ]
              , sizeClass cfg.size
              ]
            , case cfg.pointing of
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
            , case cfg.color of
                Just a ->
                    [ SemanticUI.colorClass a ]

                Nothing ->
                    []
            ]
        )
    <|
        List.concat
            [ case cfg.image of
                Just url ->
                    [ img [ src url ] [] ]

                Nothing ->
                    []
            , case cfg.icon of
                Just a ->
                    [ Icon.icon Icon.init a ]

                Nothing ->
                    []
            , content
            , case cfg.detail of
                Just a ->
                    [ div [ class "detail" ] [ Html.text a ] ]

                _ ->
                    []
            ]
