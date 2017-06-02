module SemanticUI.Collections.Message exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model msg =
    { error : Bool
    , contents : List (Html msg)
    }


init : Model msg
init =
    { error = False
    , contents = []
    }


error : Bool -> Model msg -> Model msg
error error model =
    { model | error = error }


contents : List (Html msg) -> Model msg -> Model msg
contents contents model =
    { model | contents = contents }


view : Model msg -> Html msg
view { error, contents } =
    div
        [ class "ui message"
        , classList [ ( "error", error ) ]
        ]
        contents
