module SemanticUI.Elements.Icon exposing (Icon(..), init, Config, size, icon)

{-|

An icon is a glyph used to represent something else.

# Viewing icons

@docs Icon, icon

# Icon properties

@docs Config, init

## Size

An icon can vary in size.

@docs size

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (Size(..))


{-| The name of the icon. See
[https://semantic-ui.com/elements/icon.html](https://semantic-ui.com/elements/icon.html) for a table
of options.
-}
type Icon
    = Alarm
    | AlignJustify
    | ArrowLeft
    | ArrowRight
    | CheckCircleOutline
    | ChevronUp
    | Download
    | Dropbox
    | FileOutline
    | Fork
    | GitHub
    | Help
    | Hide
    | Info
    | Legal
    | Plus
    | Retweet
    | Search
    | Print
    | Settings
    | Star
    | Trash
    | Upload
    | User
    | Warning
    | WarningSign


{-| Custom sizeClass that doesn't render medium (as the "medium" class is
Medium.com's icon)
-}
sizeClass : Size -> Attribute msg
sizeClass size =
    case size of
        Mini ->
            class "mini"

        Tiny ->
            class "tiny"

        Small ->
            class "small"

        Medium ->
            class ""

        Large ->
            class "large"

        Big ->
            class "big"

        Huge ->
            class "huge"

        Massive ->
            class "massive"


{-| Configuration of an icon.
-}
type alias Config msg =
    { size : Size
    , attributes : List (Attribute msg)
    }


{-| Specify the size of an icon.
-}
size : Size -> Config msg -> Config msg
size size model =
    { model | size = size }


{-| The most basic configuration of an icon.
-}
init : Config msg
init =
    { size = Medium
    , attributes = []
    }


{-| View an icon with a particular configuration.
-}
icon : Config msg -> Icon -> Html msg
icon { size, attributes } icon =
    i
        (List.concat
            [ attributes
            , [ class "ui icon"
              , sizeClass size
              , class <|
                    case icon of
                        Dropbox ->
                            "dropbox"

                        Warning ->
                            "warning"

                        WarningSign ->
                            "warning sign"

                        ArrowRight ->
                            "arrow right"

                        ArrowLeft ->
                            "left arrow"

                        Search ->
                            "search"

                        Plus ->
                            "plus"

                        Info ->
                            "info"

                        User ->
                            "user"

                        CheckCircleOutline ->
                            "check circle outline"

                        Help ->
                            "help"

                        ChevronUp ->
                            "chevron up"

                        Trash ->
                            "trash"

                        Upload ->
                            "upload"

                        Download ->
                            "download"

                        Fork ->
                            "fork"

                        Star ->
                            "star"

                        GitHub ->
                            "github"

                        Legal ->
                            "legal"

                        Hide ->
                            "hide"

                        Alarm ->
                            "alarm"

                        AlignJustify ->
                            "align justify"

                        Settings ->
                            "settings"

                        Retweet ->
                            "retweet"

                        Print ->
                            "print"

                        FileOutline ->
                            "file outline"
              ]
            ]
        )
        []
