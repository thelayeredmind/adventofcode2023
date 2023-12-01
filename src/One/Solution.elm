module One.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Browser
import Maybe.Extra

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
  let digits = String.toList line |> List.filter Char.isDigit
  in 
    [List.head digits,  List.head <| List.reverse digits] |>
    Maybe.Extra.values |>
    String.fromList |>
    solveInt
  


solveInt : String -> Int
solveInt value = Maybe.withDefault 0 <| String.toInt value