module SemanticUI.Collections.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model msg =
    { attributes : List (Attribute msg)
    , error : Bool
    }


init : Model msg
init =
    { attributes = []
    , error = False
    }


attributes : List (Attribute msg) -> Model msg -> Model msg
attributes attributes model =
    { model | attributes = attributes }


error : Bool -> Model msg -> Model msg
error error model =
    { model | error = error }


view : Model msg -> List (Html msg) -> Html msg
view { attributes, error } contents =
    Html.form
        (List.concat
            [ attributes
            , [ class "ui form"
              , classList [ ( "error", error ) ]
              ]
            ]
        )
        contents
