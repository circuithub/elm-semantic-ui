module SemanticUI.Elements.Label exposing
    ( label
    , init, Config
    , image
    , color
    , detail
    , pointing, Pointing(..)
    , basic
    , circular
    , size
    , attributes, horizontal, icon, link
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
type alias Config msg =
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
    , attributes : List (Attribute msg)
    }


{-| Any other custom `Attribute`s to add to this button. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a model =
    { model | attributes = a }


{-| Which direction a label can point.
-}
type Pointing
    = PointAbove
    | PointBelow
    | PointLeft
    | PointRight


{-| Specify the size of a label.
-}
size : Size -> Config msg -> Config msg
size a model =
    { model | size = a }


{-| Specify whether or not a label is circular.
-}
circular : Bool -> Config msg -> Config msg
circular a model =
    { model | circular = a }


tag : Bool -> Config msg -> Config msg
tag a model =
    { model | tag = a }


{-| Specify whether or not a label is basic.
-}
basic : Bool -> Config msg -> Config msg
basic a model =
    { model | basic = a }


{-| Specify whether a label should point to surrounding content.
-}
pointing : Maybe Pointing -> Config msg -> Config msg
pointing a model =
    { model | pointing = a }


{-| Specify the colour of a label.
-}
color : Maybe Color -> Config msg -> Config msg
color a model =
    { model | color = a }


{-| Specify extra detail to display in this label.
-}
detail : Maybe String -> Config msg -> Config msg
detail a model =
    { model | detail = a }


{-| Specify the URL to an image to display in this label.
-}
image : Maybe String -> Config msg -> Config msg
image a model =
    { model | image = a }


horizontal : Bool -> Config msg -> Config msg
horizontal a model =
    { model | horizontal = a }


icon : Maybe Icon.Icon -> Config msg -> Config msg
icon a model =
    { model | icon = a }


{-| A label with the simplest configuration. Corresponds to just `class="ui label"`.
-}
init : Config msg
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
    , attributes = []
    }


link : Config msg -> String -> String -> Html msg
link cfg url =
    viewAs (\attrs -> a (href url :: attrs)) cfg


label : Config msg -> String -> Html msg
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
            , cfg.attributes
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
                Just Icon.Close ->
                    []

                Just a ->
                    [ Icon.icon Icon.init a ]

                Nothing ->
                    []
            , content
            , case cfg.icon of
                Just Icon.Close ->
                    [ Icon.icon Icon.init Icon.Close ]

                _ ->
                    []
            , case cfg.detail of
                Just a ->
                    [ div [ class "detail" ] [ Html.text a ] ]

                _ ->
                    []
            ]
