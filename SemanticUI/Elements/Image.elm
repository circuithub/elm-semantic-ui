module SemanticUI.Elements.Image exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


type alias Model =
    { src : String
    , size : Maybe Size
    , centered : Bool
    }


init : Model
init =
    { src = ""
    , size = Nothing
    , centered = False
    }


size : Maybe Size -> Model -> Model
size size model =
    { model | size = size }


centered : Bool -> Model -> Model
centered centered model =
    { model | centered = centered }


src : String -> Model -> Model
src src model =
    { model | src = src }


image : Model -> Html msg
image { src, size, centered } =
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
