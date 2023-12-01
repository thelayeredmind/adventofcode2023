module One.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Browser
import Dict exposing (Dict)
import Maybe.Extra
import Regex

main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type Msg = CodeChanged String

type alias Model = 
  { inputCode : String
  , result : String
  }

init : Model
init =
  { inputCode = ""
  , result = "" 
  }

view : Model -> Html Msg
view model = 
  div [] 
  [ textarea 
      [onInput CodeChanged
      ]
      []
  , p [] [text model.result]

  ]


update : Msg -> Model -> Model
update msg model = 
  case msg of
    CodeChanged a ->
      {model | result = processCode a |> String.fromInt}


processCode : String -> Int
processCode change = 
  String.lines change |>
  List.map extractDigits |> 
  List.sum 

extractDigits : String -> Int
extractDigits line = 
  let 
    digits = 
      findNumbers line |> 
      Maybe.Extra.values |> 
      List.map (\a -> String.fromInt a)
  in 
    [List.head digits,  List.head <| List.reverse digits] |>
    Maybe.Extra.values |>
    String.concat |>
    solveInt



solveInt : String -> Int
solveInt value = Maybe.withDefault 0 <| String.toInt value


literalDigits = Dict.fromList
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ]
  
readNumber : String -> Maybe Int
readNumber word = Dict.get word literalDigits


findNumbers : String -> List (Maybe Int)
findNumbers line = 
  let 
    query = 
      Maybe.withDefault Regex.never <|
      Regex.fromString <|
      Debug.log "query: " (String.append 
        (String.join "|" 
          (List.map (\a -> "(" ++ a ++ ")")
          (Dict.keys literalDigits))
        ) <| "|\\d")
    matches = Regex.find query line

  in List.map 
    (\match -> 
      if (String.length match.match) > 1 
        then readNumber match.match 
      else String.toInt match.match)
    matches