module Seven.Solution exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)
import Browser
import AssocList as Dict
import List.Extra as List
import Maybe.Extra as Maybe

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
  , p 
    [] 
    <| (parse model.input |>
    List.sortWith compareHands |>
    List.indexedMap 
      (\i a -> 
        let 
          (cards, bid) = a 
        in
          p [] [text <| (String.join " | " <| List.map printCard cards) ++ "     " ++ "[" ++ String.fromInt bid ++ " x " ++ String.fromInt (i + 1) ++ "]"]
      ) |>
    List.reverse |>
    List.intersperse (br [] []))
  ]


printCard : Card -> String
printCard c = 
  case c of 
    A -> "A"
    K -> "K"
    Q -> "Q"
    J -> "J"
    Number n -> String.fromInt n

update : Msg -> Model -> Model
update msg model =
  case msg of
    OnInputChange a -> 
        { model 
        | result = (String.fromInt <| solve1 a, String.fromInt <| solve2 a)
        , input = a
        }


type alias Hand = (List Card, Int)

type Card = A | K | Q | J | Number Int

solve1 : String -> Int
solve1 str = 
  Debug.log "Products"
  (parse str |>
  List.sortWith compareHands |>
  List.indexedMap (\i a -> (i + 1) * Tuple.second a)) |>
  List.sum

solve2 : String -> Int
solve2 str = 0


compareHands : Hand -> Hand -> Order
compareHands hand1 hand2 = 
  let 
    valueHand1 = rankHand hand1
    valueHand2 = rankHand hand2
    (cards1, _) = hand1
    (cards2, _) = hand2

  in
    if valueHand1 == valueHand2
    then compare (List.map cardValue cards1) (List.map cardValue cards2)
    else compare valueHand1 valueHand2

cardValue : Card -> Int
cardValue card = 
      case card of
        Number n -> n
        J -> 11
        Q -> 12
        K -> 13
        A -> 14

rankHand : Hand -> Int
rankHand hand = 
  let
    (cards, _) = hand
    frequencies = 
      List.foldl 
        (\card matches ->
          case (Dict.get card matches) of
            Nothing -> Dict.insert card 1 matches
            Just n -> Dict.insert card (n + 1) matches
        )
        (Dict.fromList [])
        cards
    estimatePlay listOfMatchedCards =
      case listOfMatchedCards of
        -- 5 of a Kind --
        [(crd, n)] ->  10
        [(crd1, n1), (crd2, n2)] ->
          if n1 > 3
          -- 4 of a Kind ---
            then 8
          -- Full House --
            else 6
        [(set1, n1), (set2, n2), _] -> 
          if n1 > n2
          -- Three of a Kind --
          then 4
          -- Double Pair --
          else 2
          -- Pair --
        [(set1, n1), _, _, _] -> 
           1
        -- None --
        _ -> 0
  
  in
    Dict.toList frequencies |>
    List.sortBy Tuple.second |>
    List.reverse |>
    estimatePlay
      


parse : String -> List Hand
parse str =
  let
    parseCard char = 
      case char of 
        'A' -> Just A
        'K' -> Just K
        'Q' -> Just Q
        'J' -> Just J
        'T' -> Just <| Number 10
        c -> 
          String.fromList [c] |> 
          String.toInt |> 
          Maybe.andThen 
            (\a -> 
              if a > 1 && a < 10 then Just (Number a) else Nothing)
    parseHand line = 
      case (String.split " " line) of
        [a] -> (String.toList a |> List.map parseCard |> Maybe.values, 0)
        a::b::_ -> 
          ( String.toList a |> List.map parseCard |> Maybe.values, String.toInt b |> 
            Maybe.withDefault 0)
        _ -> ([], 0)
        
    lines = String.lines str
  in
    List.map parseHand lines




