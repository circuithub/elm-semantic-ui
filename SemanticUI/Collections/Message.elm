module SemanticUI.Collections.Message
    exposing
        ( Config
        , color
        , error
        , header
        , icon
        , info
        , init
        , message
        , warning
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
import SemanticUI exposing (Color)
import SemanticUI.Elements.Icon as Icon


{-| The configuration for a message.
-}
type alias Config =
    { header : Maybe String
    , icon : Maybe Icon.Icon
    , color : Maybe Color
    }


{-| The simplest configuration of a message.
-}
init : Config
init =
    { icon = Nothing
    , header = Nothing
    , color = Nothing
    }


{-| Specify the header for a message.
-}
header : Maybe String -> Config -> Config
header a model =
    { model | header = a }


{-| Specify the icon for a message.
-}
icon : Icon.Icon -> Config -> Config
icon a model =
    { model | icon = Just a }


view : List (Attribute msg) -> Config -> List (Html msg) -> Html msg
view extraAttributes cfg contents =
    let
        contentWithHeader =
            case cfg.header of
                Nothing ->
                    contents

                Just h ->
                    div [ class "header" ] [ text h ]
                        :: contents
    in
    div
        (List.concat
            [ [ class "ui message"
              , classList
                    [ ( "icon", cfg.icon /= Nothing )
                    ]
              ]
            , case cfg.color of
                Just a ->
                    [ SemanticUI.colorClass a ]

                Nothing ->
                    []
            , extraAttributes
            ]
        )
        (case cfg.icon of
            Nothing ->
                contentWithHeader

            Just a ->
                [ Icon.icon Icon.init a
                , div [ class "contents" ] contentWithHeader
                ]
        )


{-| View a message with a particular configuration.
-}
message : Config -> List (Html msg) -> Html msg
message =
    view []


{-| Specify the colour of a message.
-}
color : Maybe Color -> Config -> Config
color a model =
    { model | color = a }


{-| View a message as an error.
-}
error : Config -> List (Html msg) -> Html msg
error =
    view [ class "error" ]


{-| View a message as a warning.
-}
warning : Config -> List (Html msg) -> Html msg
warning =
    view [ class "warning" ]


{-| View a message as information.
-}
info : Config -> List (Html msg) -> Html msg
info =
    view [ class "info" ]
