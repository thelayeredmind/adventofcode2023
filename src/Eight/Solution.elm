module Eight.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style)
import Browser
import AssocList as Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import Library exposing (..)
import Loop exposing (whileJust)

main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type Msg = 
  OnInputChange String
  | OnPartButtonClick Part
type Part = One | Two

type alias Model = 
  { result : String
  , input : String
  , part: Part
  }

init : Model
init = 
  { result = ""
  , input = ""
  , part = One
  }
  

-- TODO Refactor carrying the jackrule into model --

view : Model -> Html Msg
view model = 
  div [style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "margin" "40px"]
  
  [ div [style "display" "flex", style "flex-direction" "row", style "align-items" "center", style "margin" "40px"] 
    [ button [onClick <| OnPartButtonClick One, style "background-color" (if model.part == One then "gray" else "white")] [text "Part 1"]
    , button [onClick <| OnPartButtonClick Two, style "background-color" (if model.part == Two then "gray" else "white")] [text "Part 2"]]
  , textarea 
      [style "width" "600px", style "height" "400px"
      , onInput OnInputChange
      ]
      []
  , p [style "font-size" "20px"] [text <| "Solution: " , br [] [], text model.result]
  ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnInputChange a -> 
        { model 
        | result = 
          case model.part of
            One -> String.fromInt <| solve1 a
            Two -> String.fromInt <| solve2 a
        , input = a
        }
    OnPartButtonClick p ->
      { model
      | result = 
          case p of
            One -> String.fromInt <| solve1 model.input
            Two -> String.fromInt <| solve2 model.input
      , part = p
      }
      
solve1 : String -> Int
solve1 str = 
  parse str |>
  traverseNodes |>
  List.length
  

solve2 : String -> Int
solve2 str = 0

type Direction = Left | Right

type alias Binode = (String, String)

type alias Map = 
  { instructions : List Direction
  , nodes : Dict String Binode
  }

parse : String -> Map
parse str = 
  let
    (firstRow, nodeRows) = halfOn "\n\n" str
    parseDirection c = 
      case c of 
        'L' -> Just Left
        'R' -> Just Right
        _ -> Nothing
    parseNode = listOfRegex "\\w+" >> toPair "" >> Tuple.mapSecond String.trim
  in
  { instructions = 
      String.toList firstRow |> 
      List.map parseDirection |> 
      Maybe.values
  , nodes = 
      String.lines nodeRows |>
      List.map (halfOn "=" >> (Tuple.mapBoth String.trim parseNode)) |>
      Dict.fromList
  }

traverseNodes : Map -> List String
traverseNodes map = 
  let 
    follow dir = if dir == Left then Tuple.first else Tuple.second
    findNode dir = 
      Maybe.andThen 
        (\node -> 
          Maybe.andThen 
            (\found -> Just <| follow dir found)
            (Dict.get node map.nodes))
    collect traversalData =
        let 
          nextNode dir = findNode dir (List.head nodes) 
          (ins, nodes) = traversalData
        in
          
          case ins of
            dir::rem -> 
              Maybe.andThen
                (\a ->
                  case a of
                    "ZZZ" -> Nothing
                    _ -> Just (if rem /= [] then rem else map.instructions, a :: nodes)
                ) <| (nextNode dir)
            _ -> Nothing
    in 
      Tuple.pair map.instructions ["AAA"] |> 
      whileJust collect |>
      Tuple.second
