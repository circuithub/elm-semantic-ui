module SemanticUI.Elements.Icon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


type Icon
    = Dropbox


type alias Model msg =
    { size : Size
    , attributes : List (Attribute msg)
    }


size : Size -> Model msg -> Model msg
size size model =
    { model | size = size }


init : Model msg
init =
    { size = Medium
    , attributes = []
    }


icon : Model msg -> Icon -> Html msg
icon { size, attributes } icon =
    i
        (List.concat
            [ attributes
            , [ class "ui icon"
              , sizeClass size
              , classList
                    [ ( "dropbox", icon == Dropbox )
                    ]
              ]
            ]
        )
        []
