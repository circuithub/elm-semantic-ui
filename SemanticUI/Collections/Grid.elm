module SemanticUI.Collections.Grid exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI.Collections.Grid.Column as Column


type ColumnCount
    = OneColumn
    | TwoColumns
    | ThreeColumns
    | FourColumns
    | FiveColumns
    | SixColumns
    | SevenColumns
    | EightColumns
    | NineColumns
    | TenColumns
    | ElevenColumns
    | TwelveColumns
    | ThirteenColumns
    | FourteenColumns
    | FifteenColumns
    | SixteenColumns


type alias Model =
    { padded : Bool
    , columnsPerRow : ColumnCount
    }


init : Model
init =
    { padded = False
    , columnsPerRow = SixteenColumns
    }


padded : Bool -> Model -> Model
padded padded model =
    { model | padded = padded }


columnsPerRow : ColumnCount -> Model -> Model
columnsPerRow columnsPerRow model =
    { model | columnsPerRow = columnsPerRow }


type Column msg
    = Column Column.Model (List (Html msg))


grid : Model -> List (Column msg) -> Html msg
grid { padded, columnsPerRow } columns =
    div
        [ class "ui grid"
        , classList
            [ ( "one column", columnsPerRow == OneColumn )
            , ( "padded", padded )
            ]
        ]
        (List.map (\(Column model contents) -> Column.column model contents)
            columns
        )
