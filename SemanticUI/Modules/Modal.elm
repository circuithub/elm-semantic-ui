module SemanticUI.Modules.Modal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Config =
    { fullscreen : Bool }


init : Config
init =
    { fullscreen = False }


fullscreen : Bool -> Config -> Config
fullscreen fullscreen config =
    { config | fullscreen = fullscreen }


view :
    Config
    -> Maybe
        { header : Maybe String
        , close : Maybe msg
        , content : List (Html msg)
        , actions : List (Html msg)
        }
    -> Html msg
view { fullscreen } body =
    case body of
        Nothing ->
            div [ class "ui page dimmer transition" ] []

        Just { header, content, close, actions } ->
            div [ class "ui active page dimmer transition scrolling" ]
                [ div [ class "ui active visible modal transition"
                      , classList [("fullscreen", fullscreen)]]
                    (List.concat
                        [ case close of
                            Nothing ->
                                []

                            Just msg ->
                                [ i
                                    [ class "close icon"
                                    , onClick msg
                                    ]
                                    []
                                ]
                        , case header of
                            Nothing ->
                                []

                            Just header ->
                                [ div [ class "header" ] [ text header ] ]
                        , case content of
                            [] ->
                                []

                            content ->
                                [ div [ class "content " ] content ]
                        , case actions of
                            [] ->
                                []

                            actions ->
                                [ div [ class "actions" ] actions ]
                        ]
                    )
                ]
