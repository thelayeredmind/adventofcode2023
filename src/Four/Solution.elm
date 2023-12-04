module Four.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Browser
import Regex
import Maybe.Extra
import Array

main : Program () Model Msg
main = 
  Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

type alias Card = 
  { id : String
  , left : List Int
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
  let
    list_of_all_cards = 
      String.lines str |>
      List.map parseCard
  in
  Debug.log "List of Ids: "
  (List.foldl (collectCards list_of_all_cards) [] list_of_all_cards |>
  List.map .id |>
  List.sort) |>
  List.length 


parseCard : String -> Card
parseCard str = 
  let
    only_numbers = 
        String.split ":" str |> 
        List.reverse |> 
        List.head |> 
        Maybe.withDefault ""
    match_numbers = 
      Maybe.withDefault Regex.never <|
      Regex.fromString "\\d+"
    match_id = 
      Maybe.withDefault Regex.never <|
      Regex.fromString "(?<=Card\\s)\\d+"
    parse_numbers str_n = 
      Regex.find match_numbers str_n |>
      List.map (\a -> String.toInt a.match) |>
      Maybe.Extra.values
    numberLists = String.split "|" only_numbers
  in
    { id = 
        Regex.find match_id str |>
        List.foldl (\match result -> result ++ match.match) ""
    , left = 
        List.head numberLists |>
        Maybe.withDefault "" |>
        parse_numbers
    , right = 
        List.reverse numberLists |>
        List.head |>
        Maybe.withDefault "" |>
        parse_numbers
    }

evaluate : List Card -> Card -> Int
evaluate all_cards card = 
  getCopies all_cards card |>
  List.map countPoints |>
  List.sum

countMatches : Card -> Int
countMatches card =
  List.filter (\a -> List.member a card.right) card.left |>
  List.length

countPoints : Card -> Int
countPoints card =
  countMatches card |>
  (\a -> if a > 1 then 2^(a - 1) else a)


collectCards : List Card -> Card -> List Card -> List Card
collectCards list_of_all_cards card_to_process collected  =
    let
      won_copies = getCopies list_of_all_cards card_to_process
      new_collection = List.append collected [card_to_process]
    in
      if List.length won_copies > 0
      then List.append new_collection <| List.foldl (collectCards list_of_all_cards) [] won_copies
      else new_collection

getCopies : List Card -> Card -> List Card
getCopies list_of_all_cards card =
  let
    cards_array = Array.fromList list_of_all_cards
    from_card = 
      Array.indexedMap (\i a -> (i, a)) cards_array |>
      Array.filter (\a -> Tuple.second a == card) |>
      Array.map (\a -> Tuple.first a) |>
      Array.get 0 |>
      Maybe.withDefault 0 |>
      (\a -> a + 1)
    matched_cards = (countMatches card)
    to_card = 
      from_card + matched_cards
  in
    if matched_cards >= 1
    then 
      Array.slice from_card to_card cards_array |>
      Array.toList
    else
      []