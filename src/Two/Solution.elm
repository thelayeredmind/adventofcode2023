module Two.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style)
import Browser
import AssocList as Dict exposing (Dict)
import Html.Events exposing (on)
import Maybe.Extra
import Regex
import Array


main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type Cube = Red | Blue | Green
type alias CubeSet = Dict Cube Int
type alias Game = 
  { sets : List CubeSet
  , id : Int
  }
type Msg = OnIncrement Cube | OnDecrement Cube | OnPromptUpdate String | OnCubeSetChange

type alias Model =
  { loadedCubes : CubeSet
  , gameList : String
  , result : String
  }

init : Model
init =
  { loadedCubes = Dict.fromList [(Red, 0), (Green, 0), (Blue, 0)]
  , gameList = ""
  , result = ""
  }
  

view : Model -> Html Msg
view model = 
  div [style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "margin" "40px"] 
  [ textarea 
      [style "width" "600px", style "height" "400px"
      , onInput OnPromptUpdate
      ]
      []
  , div [style "display" "flex", style "flex-direction" "row"] 
      [cubeCounter model Red
      , cubeCounter model Green
      , cubeCounter model Blue
      ]
  , p [style "font-size" "20px"] [text model.result]

  ]
writeColor : Cube -> String
writeColor color = 
  case color of
    Red -> "RED"
    Blue -> "BLUE"
    Green -> "GREEN"

readColor : String -> Maybe Cube
readColor string = 
  case string of 
    "red" -> Just Red
    "green" -> Just Green
    "blue" -> Just Blue
    _ -> Nothing

cubeCounter : Model -> Cube -> Html Msg
cubeCounter model color = 
  div [onClick OnCubeSetChange]
  [ div [style "display" "flex", style "flex-direction" "column", style "margin" "20px"] 
    [ button [onClick <| OnIncrement color] [text "+"]
    , div [style "margin" "20px"] [text (writeColor color ++ ": " ++ (String.fromInt <| getCountForColor model.loadedCubes color))]
    , button [onClick <| OnDecrement color] [text "-"]
    ]
  ]

getCountForColor : CubeSet -> Cube -> Int
getCountForColor set color = (Maybe.withDefault 0 <| Dict.get color set)

update : Msg -> Model -> Model
update msg model = 
  case msg of
    OnDecrement color -> changeCubeCount color False model
    OnIncrement color -> changeCubeCount color True model
    OnPromptUpdate a -> {model | result = String.fromInt <| processCode model a, gameList = a}
    OnCubeSetChange -> {model | result = String.fromInt <| processCode model model.gameList}


changeCubeCount : Cube -> Bool -> Model -> Model
changeCubeCount color up model = {model | loadedCubes = 
  Dict.update color 
    (\a -> Just ( Maybe.withDefault 0 a + (if up then 1 else -1))) 
    model.loadedCubes
  }

processCode : Model -> String -> Int
processCode model change = 
  String.lines change |>
  List.map parseGame |>
  (List.filter (possibleGame model.loadedCubes)) |>
  (List.map .id) |>
  List.sum 

possibleGame : CubeSet -> Game -> Bool
possibleGame cubeset game = Dict.toList cubeset |>
    List.any 
      ( \a -> 
        (collectCubesOfColor game (Tuple.first a) |> 
        List.maximum |> 
        Maybe.withDefault 0) > (Tuple.second a)) |>
    not
collectCubesOfColor : Game -> Cube -> List Int
collectCubesOfColor game cube = List.map (Dict.get cube >> Maybe.withDefault 0) game.sets

parseGame : String -> Game
parseGame string = 
  let
    queryGameId = 
      Maybe.withDefault Regex.never <|
      Regex.fromString "(?<=Game\\s)\\d*"
    gameSetList = Array.get 1 (Array.fromList <| String.split ":" string)
    matchedId = Maybe.withDefault 
            { match = ""
            , index = 0
            , number = 0
            , submatches = []
            } <| List.head <| Regex.find queryGameId string
  in
   { sets = List.map parseSetList (String.split ";" <| Maybe.withDefault "" gameSetList)
    , id = matchedId.match |> String.toInt >> Maybe.withDefault 0
    }
 
parseSetList : String -> CubeSet
parseSetList string = Dict.fromList <|
  (String.split "," string |> List.map ((String.dropLeft 1) >> resolveCubeAmount) |> Maybe.Extra.values)

resolveCubeAmount : String -> Maybe (Cube, Int)
resolveCubeAmount string = 
  let
    values = Array.fromList <| String.split " " string
    result = (readColor (Maybe.withDefault "" <| Array.get 1 values), String.toInt <| Maybe.withDefault "" <| Array.get 0 values)
  in traverseMaybe result
     

traverseMaybe : (Maybe a, Maybe b) -> Maybe (a, b)
traverseMaybe tuple = 
  case tuple of
    (Just a, Just b) -> Just (a, b)
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing