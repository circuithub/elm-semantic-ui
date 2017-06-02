module SemanticUI.Collections.Grid.Column exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (Alignment(..))


type alias Model =
    { alignment : Maybe Alignment }


alignment : Maybe Alignment -> Model -> Model
alignment alignment model =
    { model | alignment = alignment }


init : Model
init =
    { alignment = Nothing }


column : Model -> List (Html msg) -> Html msg
column { alignment } =
    div
        [ class "column"
        , classList [ ( "center aligned", alignment == Just Centered ) ]
        ]
