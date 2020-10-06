module SemanticUI.Collections.Message exposing
    ( message, error, warning, info
    , Config, init
    , header, icon, attached, close, color
    )

{-| A message displays information that explains nearby content.


# Viewing messages

@docs message, error, warning, info


# Message properties

@docs Config, init
@docs header, icon, attached, close, color

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SemanticUI exposing (Attached, Color, attachedClass)
import SemanticUI.Elements.Icon as Icon


{-| The configuration for a message.
-}
type alias Config msg =
    { header : Maybe String
    , icon : Maybe Icon.Icon
    , color : Maybe Color
    , close : Maybe msg
    , attached : Maybe Attached
    }


{-| The simplest configuration of a message.
-}
init : Config msg
init =
    { icon = Nothing
    , header = Nothing
    , color = Nothing
    , close = Nothing
    , attached = Nothing
    }


{-| Specify whether a message is attached.
-}
attached : Maybe Attached -> Config msg -> Config msg
attached a model =
    { model | attached = a }


{-| Specify the header for a message.
-}
header : Maybe String -> Config msg -> Config msg
header a model =
    { model | header = a }


{-| Specify the icon for a message.
-}
icon : Icon.Icon -> Config msg -> Config msg
icon a model =
    { model | icon = Just a }


{-| Specify the Elm message to fire when a message's close button is clicked.
-}
close : msg -> Config msg -> Config msg
close msg model =
    { model | close = Just msg }


view : List (Attribute msg) -> Config msg -> List (Html msg) -> Html msg
view extraAttributes cfg contents =
    let
        closeContent =
            case cfg.close of
                Nothing ->
                    []

                Just msg ->
                    [ Icon.icon (Icon.init |> Icon.attributes [ onClick msg ]) Icon.Close
                    ]

        headerContent =
            case cfg.header of
                Nothing ->
                    []

                Just h ->
                    [ div [ class "header" ] [ text h ] ]

        allContent =
            headerContent ++ contents
    in
    div
        (List.concat
            [ [ class "ui message"
              , classList
                    [ ( "icon", cfg.icon /= Nothing )
                    ]
              ]
            , case cfg.attached of
                Just a ->
                    [ attachedClass a ]

                Nothing ->
                    []
            , case cfg.color of
                Just a ->
                    [ SemanticUI.colorClass a ]

                Nothing ->
                    []
            , extraAttributes
            ]
        )
        (closeContent
            ++ (case cfg.icon of
                    Nothing ->
                        allContent

                    Just a ->
                        [ Icon.icon Icon.init a
                        , div [ class "contents" ] allContent
                        ]
               )
        )


{-| View a message with a particular configuration.
-}
message : Config msg -> List (Html msg) -> Html msg
message =
    view []


{-| Specify the colour of a message.
-}
color : Maybe Color -> Config msg -> Config msg
color a model =
    { model | color = a }


{-| View a message as an error.
-}
error : Config msg -> List (Html msg) -> Html msg
error =
    view [ class "error" ]


{-| View a message as a warning.
-}
warning : Config msg -> List (Html msg) -> Html msg
warning =
    view [ class "warning" ]


{-| View a message as information.
-}
info : Config msg -> List (Html msg) -> Html msg
info =
    view [ class "info" ]
