module SemanticUI.Modules.Modal exposing (Config, init, fullscreen, view)

{-| TODO

@docs Config, init, fullscreen, view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SemanticUI exposing (Size(..), sizeClass)


{-| TODO
-}
type alias Config =
    { fullscreen : Bool
    , size : Size
    }


{-| TODO
-}
init : Config
init =
    { fullscreen = False
    , size = Medium
    }


{-| TODO
-}
fullscreen : Bool -> Config -> Config
fullscreen a config =
    { config | fullscreen = a }


{-| TODO
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
