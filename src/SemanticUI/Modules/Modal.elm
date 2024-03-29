module SemanticUI.Modules.Modal exposing (view, Config, init, fullscreen, size)

{-| A modal dialog displays content that temporarily blocks interactions with
the main view of a site.

@docs view, Config, init, fullscreen, size

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SemanticUI exposing (Size(..), sizeClass)


{-| The configuration of a modal.
-}
type alias Config =
    { fullscreen : Bool
    , size : Size
    }


{-| The simplest configuration of a modal.
-}
init : Config
init =
    { fullscreen = False
    , size = Medium
    }


{-| Specify whether the modal should be displayed fullscreen.
-}
fullscreen : Bool -> Config -> Config
fullscreen a config =
    { config | fullscreen = a }


{-| Specify the size of the modal.
-}
size : Size -> Config -> Config
size s config =
    { config | size = s }


{-| View a modal with a particular configuration.
-}
view :
    Config
    ->
        Maybe
            { header : Maybe String
            , close : Maybe msg
            , content : List (Html msg)
            , actions : List (Html msg)
            }
    -> Html msg
view cfg body =
    case body of
        Nothing ->
            div [ class "ui page dimmer transition" ] []

        Just { header, content, close, actions } ->
            div [ class "ui active page dimmer transition" ]
                [ div
                    [ class "ui active visible modal transition"
                    , sizeClass cfg.size
                    , classList [ ( "fullscreen", cfg.fullscreen ) ]
                    ]
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

                            Just a ->
                                [ div [ class "header" ] [ text a ] ]
                        , case content of
                            [] ->
                                []

                            a ->
                                [ div
                                    [ class "scrolling content"
                                    ]
                                    a
                                ]
                        , case actions of
                            [] ->
                                []

                            a ->
                                [ div [ class "actions" ] a ]
                        ]
                    )
                ]
