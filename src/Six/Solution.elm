module Six.Solution exposing (..)
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

type alias Race = (Int, Int)

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

parseNums : String -> List Int
parseNums str = 
  let regexNum = Maybe.withDefault Regex.never <| Regex.fromString "\\d+"
  in
    Regex.find regexNum str |> 
    List.map .match |> 
    Maybe.traverse String.toInt |> 
    Maybe.withDefault []

solve1 : String -> Int
solve1 str = 
  parse str |>
  List.map countOptions |>
  List.product

parse : String -> List Race
parse str = 
  let
    lines = String.lines str
  in
    case lines of
      [first, second] ->  List.zip (parseNums first) (parseNums second)
      _ -> []

countOptions : Race -> Int
countOptions r = 
  let 
    (time, dist) = r
    moveRange range = 
      let 
        (min, max) = range
        newmin = min + 1
        newmax = max - 1
        newRange = 
          ( if distance time newmin <= dist then newmin else min
          , if distance time newmax <= dist then newmax else max)
      in 
        if newRange == range then range else moveRange newRange
    (minWin, maxWin) =  moveRange (0, time)
  in maxWin - minWin - 1

distance : Int -> Int -> Int
distance totalTime chargeTime = (totalTime - chargeTime) * chargeTime

solve2 : String -> Int
solve2 str = 0 

