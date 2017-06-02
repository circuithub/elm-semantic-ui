module SemanticUI.Elements.Label exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


type alias Model msg =
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


type Pointing
    = PointAbove
    | PointBelow
    | PointLeft
    | PointRight


size : Size -> Model msg -> Model msg
size size model =
    { model | size = size }


circular : Bool -> Model msg -> Model msg
circular circular model =
    { model | circular = circular }


tag : Bool -> Model msg -> Model msg
tag tag model =
    { model | tag = tag }


basic : Bool -> Model msg -> Model msg
basic basic model =
    { model | basic = basic }


pointing : Maybe Pointing -> Model msg -> Model msg
pointing pointing model =
    { model | pointing = pointing }


color : Maybe Color -> Model msg -> Model msg
color color model =
    { model | color = color }


detail : Maybe String -> Model msg -> Model msg
detail detail model =
    { model | detail = detail }


image : Maybe String -> Model msg -> Model msg
image image model =
    { model | image = image }


horizontal : Bool -> Model msg -> Model msg
horizontal horizontal model =
    { model | horizontal = horizontal }


init : Model msg
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


label : Model msg -> List (Html msg) -> Html msg
label { image, color, detail, pointing, basic, tag, circular, horizontal, size } content =
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
