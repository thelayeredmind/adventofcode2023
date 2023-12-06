module Five.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Browser
import AssocList as Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex

main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type Msg = OnInputChange String

type alias Model = 
  { result : (String, String)
  , input : String
  }

init : Model
init = 
  { result = ("", "")
  , input = ""
  }
  

view : Model -> Html Msg
view model = 
  div [style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "margin" "40px"] 
  [ textarea 
      [style "width" "600px", style "height" "400px"
      , onInput OnInputChange
      ]
      []
  , p [style "font-size" "20px"] [text <| "Solution 1: " ++ (Tuple.first model.result)]
  , p [style "font-size" "20px"] [text <| "Solution 2: " ++ (Tuple.second model.result)]
  ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnInputChange a -> 
        { model 
        | result = (String.fromInt <| solve1 a, String.fromInt <| solve2 a)
        , input = a
        }


parseNum : String -> List Int
parseNum str = 
  let regexNum = Maybe.withDefault Regex.never <| Regex.fromString "\\d+"
  in
    Regex.find regexNum str |> 
    List.map .match |> 
    Maybe.traverse String.toInt |> 
    Maybe.withDefault []


solve1 : String -> Int
solve1 str = 
  let
      blocks = String.split "\n\n" str
      seeds = List.head blocks |> Maybe.withDefault "" |> parseNum
      mappings = List.tail blocks |> Maybe.withDefault []
  in
    seeds |>
    List.map (traverseMapping mappings) |>
    List.minimum |>
    Maybe.withDefault 0

traverseMapping : List String -> Int -> Int
traverseMapping maps seed = List.foldl mapId (Just seed) maps |> Maybe.withDefault 0
  
mapId : String -> Maybe Int -> Maybe Int
mapId mapping in_id =
  let rows = String.lines mapping |> List.tail |> Maybe.withDefault []
  in
    List.foldl findInRow in_id rows

type alias Range = (Int, Int)
type alias Mapping = (Range, Range)

calculateMapping : (Int, Int, Int) -> Mapping
calculateMapping t =
  let
    (to, from, range) = t
  in 
    ((from, from + range - 1), (to, to + range - 1))

findInRow : String -> Maybe Int -> Maybe Int
findInRow row id = 
  let 
    numbers = parseNum (Debug.log "row" row)
  in
    Debug.log "Result in Row: "
    (Maybe.andThen (\real_id ->
    case numbers of
      [to, from, range] ->
        if real_id >= from && real_id <= from + range - 1
        then Just (real_id - from + to)
        else Nothing
      _ -> Nothing)
    (Debug.log "With -> " id) )

solve2 : str -> Int
solve2 str = 0 



