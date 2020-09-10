module SemanticUI.Elements.Icon exposing
    ( Icon(..), icon
    , Config, init
    , size
    , Flip(..), flip
    , Rotate(..), rotate
    , bordered, circular, link, color, inverted, attributes
    )

{-| An icon is a glyph used to represent something else.


# Viewing icons

@docs Icon, icon


# Icon properties

@docs Config, init


## Size

An icon can vary in size.

@docs size


## Flip

An icon can be flipped horizontally or vertically

@docs Flip, flip


## Flip

An icon can be rotated

@docs Rotate, rotate


## Other properties

@docs bordered, circular, link, color, inverted, attributes

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, classList)
import SemanticUI exposing (Color(..), Size(..), colorClass)


{-| The name of the icon. See
<https://semantic-ui.com/elements/icon.html> for a table
of options.
-}
type Icon
    = Alarm
    | AlignJustify
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | ArrowsAlternate
    | Calendar
    | Camera
    | CaretDown
    | CheckCircleOutline
    | ChevronUp
    | CircleOutline
    | Close
    | Cog
    | DotCircleOutline
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
    | Industry
    | Info
    | InfoCircle
    | Legal
    | PauseCircleOutline
    | Pencil
    | Play
    | Plus
    | Print
    | QuestionCircleOutline
    | Remove
    | Retweet
    | Save
    | Search
    | Settings
    | Sort
    | SortDown
    | SortUp
    | Star
    | Stop
    | Stopwatch
    | TimesCircleOutline
    | Trash
    | ThumbsUpOutline
    | Upload
    | User
    | Warning
    | WarningSign


type Flip
    = NoFlip
    | Vertically
    | Horizontally


type Rotate
    = NoRotation
    | Clockwise
    | Counterclockwise


{-| Configuration of an icon.
-}
type alias Config msg =
    { size : Size
    , attributes : List (Attribute msg)
    , link : Bool
    , flip : Flip
    , rotate : Rotate
    , circular : Bool
    , bordered : Bool
    , inverted : Bool
    , color : Maybe Color
    }


{-| The most basic configuration of an icon.
-}
init : Config msg
init =
    { size = Medium
    , attributes = []
    , link = False
    , flip = NoFlip
    , rotate = NoRotation
    , circular = False
    , bordered = False
    , inverted = False
    , color = Nothing
    }


{-| Specify whether an icon should be flipped horizontally or vertically
-}
flip : Flip -> Config msg -> Config msg
flip flip_ cfg =
    { cfg | flip = flip_ }


{-| Specify the rotation of an icon.
-}
rotate : Rotate -> Config msg -> Config msg
rotate rotate_ cfg =
    { cfg | rotate = rotate_ }


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


{-| Specify whether or not this icon should be formatted to appear circular
-}
circular : Bool -> Config msg -> Config msg
circular x cfg =
    { cfg | circular = x }


{-| Specify whether or not this icon should be formatted to appear bordered.

> In 0.x.x bordered was formally known as squared

-}
bordered : Bool -> Config msg -> Config msg
bordered x cfg =
    { cfg | bordered = x }


{-| Specify whether or not this icon should have its colors inverted.
-}
inverted : Bool -> Config msg -> Config msg
inverted x cfg =
    { cfg | inverted = x }


{-| Specify whether or not this icon should be colored
-}
color : Maybe Color -> Config msg -> Config msg
color x cfg =
    { cfg | color = x }


attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


getFlipClass : Config msg -> ( String, Bool )
getFlipClass cfg =
    case cfg.flip of
        NoFlip ->
            ( "", False )

        Vertically ->
            ( "vertically flipped", True )

        Horizontally ->
            ( "horizontally flipped", True )


getRotationClass : Config msg -> ( String, Bool )
getRotationClass cfg =
    case cfg.rotate of
        NoRotation ->
            ( "", False )

        Clockwise ->
            ( "clockwise rotated", True )

        Counterclockwise ->
            ( "counterclockwise rotated", True )


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


{-| View an icon with a particular configuration.
-}
icon : Config msg -> Icon -> Html msg
icon cfg theIcon =
    Html.i
        (List.concat
            [ cfg.attributes
            , [ classList
                    [ ( "link", cfg.link )
                    , ( "icon", True )
                    , getFlipClass cfg
                    , getRotationClass cfg
                    , ( "circular", cfg.circular )
                    , ( "bordered", cfg.bordered )
                    , ( "inverted", cfg.inverted )
                    ]
              , sizeClass cfg.size
              , cfg.color |> Maybe.map colorClass |> Maybe.withDefault (class "")
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
                            "arrow left"

                        ArrowUp ->
                            "arrow up"

                        ArrowDown ->
                            "arrow down"

                        Search ->
                            "search"

                        Plus ->
                            "plus"

                        Info ->
                            "info"

                        InfoCircle ->
                            "info circle"

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

                        Close ->
                            "close"

                        Remove ->
                            "remove"

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

                        DotCircleOutline ->
                            "dot circle outline"

                        PauseCircleOutline ->
                            "pause circle outline"

                        ThumbsUpOutline ->
                            "thumbs up outline"

                        TimesCircleOutline ->
                            "times circle outline"

                        CircleOutline ->
                            "circle outline"

                        Camera ->
                            "camera"

                        ArrowsAlternate ->
                            "arrows alternate"

                        QuestionCircleOutline ->
                            "question circle outline"

                        Calendar ->
                            "calendar"

                        Play ->
                            "play"

                        Stop ->
                            "stop"

                        Stopwatch ->
                            "stopwatch"

                        Industry ->
                            "industry"

                        Cog ->
                            "cog"

                        Save ->
                            "save"
              ]
            ]
        )
        []
