module SemanticUI.Modules.Checkbox exposing (Config, checkbox, disabled, init, label, radio)

{-| A checkbox allows a user to select a value from a small set of options.

@docs checkbox, Config, init, label, disabled, radio

-}

import Html exposing (..)
import Html.Attributes exposing (class, classList, type_)
import Html.Events
import Maybe.Extra as Maybe


{-| The configuration of a checkbox.
-}
type alias Config msg =
    { onClick : msg
    , label : Maybe String
    , disabled : Bool
    , radio : Bool
    }


{-| The simplest configuration of a checkbox.
-}
init : { onClick : msg } -> Config msg
init { onClick } =
    { onClick = onClick
    , label = Nothing
    , disabled = False
    , radio = False
    }


{-| Specify the label for the checkbox.
-}
label : String -> Config msg -> Config msg
label lbl config =
    { config | label = Just lbl }


{-| Specify whether the label is disabled.
-}
disabled : Bool -> Config msg -> Config msg
disabled a model =
    { model | disabled = a }


{-| Specify whether the checkbox is a radio button.
-}
radio : Bool -> Config msg -> Config msg
radio a model =
    { model | radio = a }


{-| View a checkbox with a particular configuration.
-}
checkbox : Config msg -> { r | checked : Bool } -> Html msg
checkbox cfg { checked } =
    div
        [ class "ui checkbox"
        , classList
            [ ( "checked", checked )
            , ( "disabled", cfg.disabled )
            , ( "radio", cfg.radio )
            ]
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
