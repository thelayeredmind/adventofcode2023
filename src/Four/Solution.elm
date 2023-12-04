module Four.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Browser
import Regex
import Maybe.Extra

main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type alias Card = 
  { left : List Int
  , right : List Int
  }

type Msg = OnInputChange String

type alias Model = 
  { result : String
  , input : String
  }

init : Model
init = 
  { result = ""
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
  , p [style "font-size" "20px"] [text model.result]
  ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnInputChange a -> 
      { model 
      | result = String.fromInt <| solve a
      , input = a
      }


solve : String -> Int
solve str =
  Debug.log "Cards: "
  (String.lines str |>
  List.map (\a -> String.split ":" a |> List.reverse |> List.head |> Maybe.withDefault "") |>
  List.map parseCard) |>
  List.map countMatches |>
  List.sum 


parseCard : String -> Card
parseCard str = 
  let
    match_numbers = 
      Maybe.withDefault Regex.never <|
      Regex.fromString "\\d+"
    parse_numbers str_n = 
      Regex.find match_numbers str_n |>
      List.map (\a -> String.toInt a.match) |>
      Maybe.Extra.values
    numberLists = String.split "|" str
  in
    { left = 
        List.head numberLists |>
        Maybe.withDefault "" |>
        parse_numbers
    , right = 
        List.reverse numberLists |>
        List.head |>
        Maybe.withDefault "" |>
        parse_numbers
    }

countMatches : Card -> Int
countMatches card =
  List.filter (\a -> List.member a card.right) card.left |>
  List.length |>
  (\a -> if a > 1 then 2^(a - 1) else a)


