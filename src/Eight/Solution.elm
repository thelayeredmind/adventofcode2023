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
  let
      endNode = 
        Regex.fromString "ZZZ" |>
        Maybe.withDefault Regex.never
  in
    parse str |>
    traverseNodes "AAA" endNode |>
    List.length
  

solve2 : String -> Int
solve2 str = 
  let
    startNode = (Regex.fromString "\\w+A" |>
      Maybe.withDefault Regex.never)
    endNode = (Regex.fromString "\\w+Z" |>
      Maybe.withDefault Regex.never)
    map = parse str
  in
  map.nodes |>
  Dict.keys |>
  List.filter 
    ( Regex.contains
      startNode
    ) |>
  traverseNodesSim endNode map

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

traverseNodes : String -> Regex.Regex -> Map -> List String
traverseNodes entry dest map = 
    Tuple.pair map.instructions [entry] |> 
    whileJust 
        (\traversalData ->
        let 
            (ins, nodes) = traversalData
        in
            case ins of
            dir::rem -> 
                Maybe.andThen
                (\a ->
                    if Regex.contains dest a
                    then Debug.log ("End Node: " ++ a) Nothing
                    else Just (if rem /= [] then rem else map.instructions, a :: nodes)
                ) <| (nextNode map.nodes (List.head nodes) dir)
            _ -> Nothing
            ) |>
        Tuple.second

follow dir = if dir == Left then Tuple.first else Tuple.second

findNode nodes dir = 
    Maybe.andThen 
        (\node -> 
        Maybe.andThen 
            (\found -> Just <| follow dir found)
            (Dict.get node nodes))

nextNode mapNodes nodeIndex dir = findNode mapNodes dir nodeIndex 

-- #TODO Look into LCM
traverseNodesSim : Regex.Regex -> Map -> List String -> Int
traverseNodesSim dest map entries = 
    let 
        (final_ins, endnodes, final_count) =
            (map.instructions, List.map Just entries, 1) |> 
            whileJust 
                (\traversalData ->
                let 
                    (ins, lastNodes, count) = traversalData
                in
                    case ins of
                    dir::rem ->
                        let 
                            nodes = 
                                lastNodes |>
                                List.map 
                                    (\node ->
                                    Maybe.andThen
                                        (\valid_node ->
                                            -- Debug.log "Next node"
                                            ((nextNode map.nodes ( Just valid_node) dir) |> -- (Debug.log "This node" <| )
                                            Maybe.andThen Just)
                                        ) node)
                            result = 
                                nodes |> 
                                Maybe.values |> 
                                List.map (Regex.contains dest) |>
                                List.all identity
                        in
                            -- Debug.log ("(" ++ Debug.toString (List.length rem) ++ ")" ++ " " ++ Debug.toString dir ++ "\n\n")
                            -- (
                            if
                                result 
                                    then Nothing
                                    else Just (if rem /= [] then rem else map.instructions, nodes, count + 1)
                            -- )
                    _ -> Nothing) 
    in final_count
