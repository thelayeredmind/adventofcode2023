module Three.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Browser
import Array
import Maybe.Extra

main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Position = {x : Int, y : Int}
type CharType = Special Char | Digit Char
type alias CellContent = Maybe CharType
type alias Cell = {pos : Position, content : CellContent}
type alias Grid = 
  { cells : List Cell
  , width : Int
  , height : Int
  }

type Msg = OnInputChange String

type alias Model = 
  { result : String
  , input : String
  , grid : Grid
  , groups : List (List Cell)
  }

init : Model
init = 
  { result = ""
  , input = ""
  , grid = 
      { cells = []
      , width = 0
      , height = 0
      }
  , groups = []
  }
  

view : Model -> Html Msg
view model = 
  div [style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "margin" "40px"] 
  [ textarea 
      [style "width" "600px", style "height" "400px"
      , onInput OnInputChange
      ]
      []
  , p [style "font-size" "20px"] [text model.result]
  , displayGrid model.grid model.groups
  ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnInputChange a -> 
      let
        inputGrid = parseGrid a
        connected = getConnectedDigitGroups inputGrid
      in
      { model 
      | result = String.fromInt <| sumGroups connected
      , input = a
      , grid = inputGrid
      , groups = connected
      }

parseGrid : String -> Grid
parseGrid str = 
  let
    rows = String.lines str |>
      List.indexedMap parseRow
    allcells = List.concat rows
    gridwidth = List.length <| Maybe.withDefault [] <| List.head rows
    gridheight = List.length rows
  in
    { cells = allcells
    , width = gridwidth
    , height = gridheight
    }

parseRow : Int -> String -> List Cell
parseRow rowId str =
  String.toList str |>
  List.indexedMap (parseCell rowId)

parseCell : Int -> Int -> Char -> Cell
parseCell rowId posOnRow char =
  let 
    position = {x = posOnRow, y = rowId}
    inside = 
      if Char.isDigit char then Just <| Digit char
      else if char == '.' then Nothing
      else Just <| Special char
  in
  { pos = position, content = inside }

--------------------------------

displayGrid : Grid -> List (List Cell) -> Html msg
displayGrid grid connected = 
  let 
    rows = 
      Array.fromList grid.cells |>
      splitArrayEvery grid.width []
  in
    div 
      [] <|
      List.map (displayRow connected) rows

-- Must be recursive
splitArrayEvery : Int -> List (List a) -> Array.Array a -> List (List a)
splitArrayEvery every listoflists array = 
    let
        remaining = (Array.length array) - every
    in
      if remaining <= 0 then List.append listoflists <| [Array.toList array]
      else 
        let
          newList = List.append listoflists <| [Array.toList <| Array.slice 0 (every) array]
          leftOver = Array.slice every (Array.length array) <| array
        in
          splitArrayEvery every newList leftOver
        


displayRow : List (List Cell) -> List Cell -> Html msg
displayRow connected cellsInRow = 
  let
    num = List.length cellsInRow
    maxWidth = (1800 // num)
  in
    div
      [style "display" "flex", style "flex-direction" "row"] <|
      List.map (displayCell maxWidth connected) cellsInRow 

displayCell : Int -> List (List Cell) -> Cell -> Html msg
displayCell width connected cell = 
    let
        cellStyle = 
          case cell.content of
            Nothing -> [style "background-color" "grey"]
            Just (Digit _) -> 
              [ style "background-color" <|
                if List.any (\a -> List.member cell a) connected
                then "green"
                else "black"
              , style "color" "white"
              ]
            Just (Special _) -> [style "background-color" "yellow"]
        content = 
          case cell.content of
            Nothing -> ""
            Just (Digit c) -> String.fromList [c]
            Just (Special c) -> String.fromList [c]
    in
    
    div 
      ( List.append 
          [ style "width" <| (String.fromInt width) ++ "px"
          , style "height" <| (String.fromInt width) ++ "px"
          , style "text-align" "center"
          , style "padding" "2px"
          , style "border" "1px solid black"
          ] 
          cellStyle)
      [text content]

----------------------------------------------------------------

sumGroups : List (List Cell) -> Int
sumGroups connected = 
  connected |>
  List.map groupToInt |>
  Maybe.Extra.values |>
  List.sum

getConnectedDigitGroups : Grid -> List (List Cell)
getConnectedDigitGroups grid = 
  groupDigits grid |>
  List.filter (\a -> 
    List.any (\cell -> 
      getNeighbors grid cell  |>
      Maybe.Extra.values |>
      List.map .content |>
      List.any isCellSpecial)
    a
  ) 

groupDigits: Grid -> List (List Cell)
groupDigits grid = 
  List.filter (\a -> (isCellDigit a.content)
    &&
      let neighbor = getNeighbor grid a (-1, 0)
      in
       ( neighbor == Nothing || not (isCellDigit <| Maybe.withDefault Nothing <| Maybe.map .content neighbor))) grid.cells |>
  List.map (\a -> scanLineForDigits grid [a])

scanLineForDigits : Grid -> List Cell -> List Cell
scanLineForDigits grid list =
  let rightOf = getNeighbor grid (Maybe.withDefault {pos = {x = -2, y = -2}, content = Nothing } (List.reverse list |> List.head)) (1, 0)
  in
    case rightOf of
      Nothing -> list
      Just cell -> 
        if not (isCellDigit cell.content) 
        then list 
        else scanLineForDigits grid <| List.append list [cell]

groupToInt : List Cell -> Maybe Int
groupToInt groupOfDigits = 
    List.map .content groupOfDigits |>
    List.map unwrapDigit  |>
    Maybe.Extra.values |>
    String.fromList |>
    String.toInt

unwrapDigit : Maybe CharType -> Maybe Char
unwrapDigit wrappedDigit =
  Maybe.andThen (\a ->
      case a of 
        (Digit c) -> Just c
        _ -> Nothing) wrappedDigit
    

isCellDigit : Maybe CharType -> Bool
isCellDigit content =
  case content of
    Just (Digit _) -> True
    _ -> False

isCellSpecial : Maybe CharType -> Bool
isCellSpecial content =
  case content of
    Just (Special _) -> True
    _ -> False

getNeighbors : Grid -> Cell -> List (Maybe Cell)
getNeighbors grid cell = 
  List.map (getNeighbor grid cell) <|
      [ (-1, -1), (0, -1), (1, -1)
      , (-1, 0),            (1, 0)
      , (-1, 1),  (0, 1),  (1, 1)
      ]


getNeighbor : Grid -> Cell -> (Int, Int)  -> Maybe (Cell)
getNeighbor grid cell offset = getCell grid {x = cell.pos.x + (Tuple.first offset), y = cell.pos.y + (Tuple.second offset)}


getCell : Grid -> Position -> Maybe (Cell)
getCell grid pos = List.head <| List.filter (\a -> a.pos == pos) grid.cells 