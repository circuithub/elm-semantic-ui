module SemanticUI.Elements.Icon
    exposing
        ( Config
        , Icon(..)
        , attributes
        , icon
        , init
        , link
        , size
        )

{-| An icon is a glyph used to represent something else.


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
[<https://semantic-ui.com/elements/icon.html>](https://semantic-ui.com/elements/icon.html) for a table
of options.
-}
type Icon
    = Alarm
    | AlignJustify
    | ArrowLeft
    | ArrowRight
    | CaretDown
    | CheckCircleOutline
    | ChevronUp
    | Download
    | Dropbox
    | Dropdown
    | FileOutline
    | Filter
    | Fork
    | GitHub
    | Help
    | Hide
    | ImageOutline
    | Info
    | Legal
    | Pencil
    | Plus
    | Print
    | Retweet
    | Search
    | Settings
    | Sort
    | SortDown
    | SortUp
    | Star
    | Trash
    | Upload
    | User
    | Warning
    | WarningSign


attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


{-| Custom sizeClass that doesn't render medium (as the "medium" class is
Medium.com's icon)
-}
sizeClass : Size -> Attribute msg
sizeClass a =
    case a of
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
    , link : Bool
    }


{-| Specify the size of an icon.
-}
size : Size -> Config msg -> Config msg
size a model =
    { model | size = a }


{-| Specify whether or not this is a link icon.
-}
link : Bool -> Config msg -> Config msg
link a model =
    { model | link = a }


{-| The most basic configuration of an icon.
-}
init : Config msg
init =
    { size = Medium
    , attributes = []
    , link = False
    }


{-| View an icon with a particular configuration.
-}
icon : Config msg -> Icon -> Html msg
icon cfg theIcon =
    i
        (List.concat
            [ cfg.attributes
            , [ classList
                    [ ( "link", cfg.link )
                    , ( "icon", True )
                    ]
              , sizeClass cfg.size
              , class <|
                    case theIcon of
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

                        CaretDown ->
                            "caret down"

                        Dropdown ->
                            "dropdown"

                        SortUp ->
                            "sort up"

                        SortDown ->
                            "sort down"

                        Sort ->
                            "sort"

                        Pencil ->
                            "pencil alternative"

                        Filter ->
                            "filter"

                        ImageOutline ->
                            "image outline"
              ]
            ]
        )
        []
