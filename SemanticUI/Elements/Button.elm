module SemanticUI.Elements.Button exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


type Emphasis
    = Primary
    | Secondary


type Animation
    = DefaultAnimation
    | VerticalAnimation
    | FadeAnimation


type alias HiddenContent msg =
    { hiddenContent : List (Html msg)
    , animation : Animation
    }


type alias Model msg =
    { emphasis : Maybe Emphasis
    , hiddenContent : Maybe (HiddenContent msg)
    , basic : Bool
    , inverted : Bool
    , loading : Bool
    , fluid : Bool
    , attributes : List (Attribute msg)
    , size : Size
    }


fluid : Bool -> Model msg -> Model msg
fluid fluid model =
    { model | fluid = fluid }


size : Size -> Model msg -> Model msg
size size model =
    { model | size = size }


attributes : List (Attribute msg) -> Model msg -> Model msg
attributes attrs model =
    { model | attributes = attrs }


init : Model msg
init =
    { emphasis = Nothing
    , hiddenContent = Nothing
    , inverted = False
    , basic = False
    , fluid = False
    , loading = False
    , attributes = []
    , size = Medium
    }


basic : Bool -> Model msg -> Model msg
basic basic model =
    { model | basic = basic }


loading : Bool -> Model msg -> Model msg
loading loading model =
    { model | loading = loading }


inverted : Bool -> Model msg -> Model msg
inverted inverted model =
    { model | inverted = inverted }


emphasis : Maybe Emphasis -> Model msg -> Model msg
emphasis emphasis model =
    { model | emphasis = emphasis }


hiddenContent :
    Maybe { animation : Animation, hiddenContent : List (Html msg) }
    -> Model msg
    -> Model msg
hiddenContent hiddenContent model =
    { model | hiddenContent = hiddenContent }

button = viewAs Html.button

link = viewAs a

viewAs element { emphasis, hiddenContent, basic, inverted, loading, fluid, attributes, size } label =
    element
        (List.concat
            [ attributes
            , [ class "ui button"
              , classList
                    [ ( "primary", emphasis == Just Primary )
                    , ( "secondary", emphasis == Just Secondary )
                    , ( "basic", basic )
                    , ( "inverted", inverted )
                    , ( "loading", loading )
                    , ( "fluid", fluid )
                    , ( "inverted", inverted )
                    ]
              , sizeClass size
              ]
            , case hiddenContent of
                Nothing ->
                    []

                Just { animation } ->
                    List.concat
                        [ [ class "animated" ]
                        , case animation of
                            DefaultAnimation ->
                                []

                            VerticalAnimation ->
                                [ class "vertical" ]

                            FadeAnimation ->
                                [ class "fade" ]
                        ]
            ]
        )
    <|
        case hiddenContent of
            Nothing ->
                label

            Just { hiddenContent } ->
                [ div [ class "visible content" ] label
                , div [ class "hidden content" ] hiddenContent
                ]
