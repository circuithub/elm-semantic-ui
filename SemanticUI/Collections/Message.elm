module SemanticUI.Collections.Message exposing
    ( message, error, warning
    , init, Config
    , header
    , icon
    , attached, close, color, info
    )

{-| A message displays information that explains nearby content.


# Viewing messages

@docs message, error, warning


# Message properties

@docs init, Config


## Header

A message can have a header.

@docs header


## Icon

A message can contain an icon.

@docs icon

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
