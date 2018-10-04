module SemanticUI.Modules.Checkbox exposing (Config, checkbox, init, label)

import Html exposing (..)
import Html.Attributes exposing (class, classList, type_)
import Html.Events
import Maybe.Extra as Maybe


type alias Config msg =
    { onClick : msg
    , label : Maybe String
    }


init : { onClick : msg } -> Config msg
init { onClick } =
    { onClick = onClick
    , label = Nothing
    }


label : String -> Config msg -> Config msg
label lbl config =
    { config | label = Just lbl }


checkbox : Config msg -> { r | checked : Bool } -> Html msg
checkbox cfg { checked } =
    div
        [ class "ui checkbox"
        , classList [ ( "checked", checked ) ]
        , Html.Events.onClick cfg.onClick
        ]
        [ input
            [ type_ "checkbox"
            , Html.Attributes.checked checked
            , class "hidden"
            ]
            []
        , Html.label []
            (cfg.label
                |> Maybe.map text
                |> Maybe.toList
            )
        ]
