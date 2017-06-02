module SemanticUI.Collections.Message
    exposing
        ( message
        , error
        , warning
        , init
        , Config
        , header
        , icon
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
import SemanticUI.Elements.Icon as Icon


{-| The configuration for a message.
-}
type alias Config =
    { header : Maybe String
    , icon : Maybe Icon.Icon
    }


{-| The simplest configuration of a message.
-}
init : Config
init =
    { icon = Nothing
    , header = Nothing
    }


{-| Specify the header for a message.
-}
header : Maybe String -> Config -> Config
header header model =
    { model | header = header }


{-| Specify the icon for a message.
-}
icon : Icon.Icon -> Config -> Config
icon icon model =
    { model | icon = Just icon }


view : List (Attribute msg) -> Config -> List (Html msg) -> Html msg
view extraAttributes { icon, header } contents =
    let
        contentWithHeader =
            case header of
                Nothing ->
                    contents

                Just header ->
                    div [ class "header" ] [ text header ]
                        :: contents
    in
        div
            (List.concat
                [ [ class "ui message"
                  , classList
                        [ ( "icon", icon /= Nothing )
                        ]
                  ]
                , extraAttributes
                ]
            )
            (case icon of
                Nothing ->
                    contentWithHeader

                Just icon ->
                    [ Icon.icon Icon.init icon
                    , div [ class "contents" ] contentWithHeader
                    ]
            )


{-| View a message with a particular configuration.
-}
message : Config -> List (Html msg) -> Html msg
message =
    view []


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
