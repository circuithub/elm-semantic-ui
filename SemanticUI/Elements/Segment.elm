module SemanticUI.Elements.Segment exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model msg =
    { raised : Bool
    , attributes : List (Attribute msg)
    }


init : Model msg
init =
    { raised = False
    , attributes = []
    }


raised : Bool -> Model msg -> Model msg
raised raised model =
    { model | raised = raised }


attributes : List (Attribute msg) -> Model msg -> Model msg
attributes attributes model =
    { model | attributes = attributes }


segment : Model msg -> List (Html msg) -> Html msg
segment { raised, attributes } =
    div
        (List.concat
            [ attributes
            , [ class "ui segment"
              , classList [ ( "raised", raised ) ]
              ]
            ]
        )
