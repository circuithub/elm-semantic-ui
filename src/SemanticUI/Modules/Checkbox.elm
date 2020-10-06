module SemanticUI.Modules.Checkbox exposing (Config, checkbox, disabled, init, label, radio)

{-| TODO

@docs Config, checkbox, disabled, init, label, radio

-}

import Html exposing (..)
import Html.Attributes exposing (class, classList, type_)
import Html.Events
import Maybe.Extra as Maybe


{-| TODO
-}
type alias Config msg =
    { onClick : msg
    , label : Maybe String
    , disabled : Bool
    , radio : Bool
    }


{-| TODO
-}
init : { onClick : msg } -> Config msg
init { onClick } =
    { onClick = onClick
    , label = Nothing
    , disabled = False
    , radio = False
    }


{-| TODO
-}
label : String -> Config msg -> Config msg
label lbl config =
    { config | label = Just lbl }


{-| TODO
-}
disabled : Bool -> Config msg -> Config msg
disabled a model =
    { model | disabled = a }


{-| TODO
-}
radio : Bool -> Config msg -> Config msg
radio a model =
    { model | radio = a }


{-| TODO
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
