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

type Thing = 
    Seed 
  | Soil 
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location

type alias Mapping = Dict (Thing, Thing) (Dict Int Int)

type alias Range = ((Int, Int), Int)

type Msg = OnInputChange String

type alias Model = 
  { result : (String, String)
  , input : String
  }

type alias Structure = 
  { seeds : List Int
  , mappings : Mapping
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
      let structure = parse a
      in
        { model 
        | result = (String.fromInt <| solve1 structure, String.fromInt <| solve2 structure)
        , input = a
        }


parse : String -> Structure
parse str = 
  let
    blocks = String.split "\n\n" str
    seedsBlock = List.head blocks |> Maybe.withDefault ""
    mapps = Dict.fromList <| Maybe.values <| List.map (parseMapping (parseNum str |> List.maximum |> Maybe.withDefault 0)) <| Maybe.withDefault [] <| List.tail blocks
  in
  { seeds = parseNum seedsBlock
  , mappings = mapps
  }


parseNum : String -> List Int
parseNum str = 
  let regexNum = Maybe.withDefault Regex.never <| Regex.fromString "\\d+"
  in
    Regex.find regexNum str |> 
    List.map .match |> 
    Maybe.traverse String.toInt |> 
    Maybe.withDefault []


listPairMap : (a -> (b, c)) -> List a -> (List b, List c)
listPairMap f l = List.foldl (\a b -> 
  let
    (firstList, secondList) = b
    (result1, result2) = f a
  in
    ( List.append firstList <| [result1]
    , List.append secondList <| [result2])
    ) ([], []) l


parseMapping : Int -> String -> Maybe ((Thing, Thing), Dict Int Int)
parseMapping maxNum str = 
  let
    lines = String.lines str
    title = 
      List.head lines |> 
      Maybe.withDefault "" |> 
      String.split " " |>
      List.head |>
      Maybe.withDefault "" |> 
      String.split "-to-" |> 
      listToPair |> 
      Maybe.withDefault ("", "") |> 
      tupleMap parseThing |> 
      traversePair
    ranges = 
      List.tail lines |> 
      Maybe.withDefault [] |> 
      List.map parseNum |>
      List.map listToRange |>
      Maybe.values |>
      List.map rangeToMapping |>
      List.concat
    fullMapping = 
      List.zip
        (List.range 0 maxNum |> List.filterNot (existingValues ranges Tuple.first))
        (List.range 0 maxNum |> List.filterNot (existingValues ranges Tuple.second)) |> 
      List.append ranges
  in
    Maybe.andThen 
      (\a -> Just (a, Dict.fromList fullMapping))
      title

existingValues : List (Int, Int) -> ((Int, Int) -> Int) -> Int -> Bool
existingValues ranges accessor n = List.member n <| List.map accessor ranges

traversePair : (Maybe a, Maybe a) -> Maybe (a, a)
traversePair t = 
  case t of
    (Just a, Just b) -> Just (a, b)
    _ -> Nothing

listToPair : List a -> Maybe (a, a)
listToPair l =
  case l of
    [a, b] -> Just (a, b)
    _ -> Nothing

listToRange : List Int -> Maybe Range
listToRange l =
  case l of
    [a, b, c] -> Just ((a, b), c)
    _ -> Nothing

rangeToMapping : Range -> List (Int, Int)
rangeToMapping r =
  let ((to, from), range) = r
  in
    List.zip (List.range from (from + range - 1)) (List.range to (to + range - 1))

tupleMap : (a -> b) -> (a, a) -> (b, b)
tupleMap f t = 
  let 
    (a, b) = t
  in (f a, f b)

parseThing : String -> Maybe Thing
parseThing str = 
  case str of
    "seed" -> Just Seed
    "soil" -> Just Soil
    "fertilizer" -> Just Fertilizer
    "water" -> Just Water
    "light" -> Just Light
    "temperature" -> Just Temperature
    "humidity" -> Just Humidity
    "location" -> Just Location
    _ -> Nothing

solve1 : Structure -> Int
solve1 str = 
  List.map (traverse str.mappings) str.seeds |>
  Maybe.values |>
  List.minimum |>
  Maybe.withDefault 0

solve2 : Structure -> Int
solve2 str = 0 

traverse : Mapping -> Int -> Maybe Int
traverse maps s = 
  List.foldl mapId (Just s) <| Dict.values maps

mapId : Dict Int Int -> Maybe Int -> Maybe Int
mapId dic index = 
  Maybe.andThen (\i -> Dict.get  i (Debug.log "on: " dic)) (Debug.log "Trying with: " index)

