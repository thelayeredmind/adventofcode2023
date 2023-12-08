module Library exposing (..)
import Regex
import AssocList as Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Maybe exposing (Maybe)

parseNums : String -> List Int
parseNums = 
  listOfRegex "\\d+" >> List.map String.toInt >> Maybe.values

listOfRegex : String -> String -> List String
listOfRegex pattern = 
  let regex = Maybe.withDefault Regex.never <| Regex.fromString pattern
  in List.map .match << Regex.find regex
  
  
halfOn : String -> String -> (String, String)
halfOn on = String.split on >> toPair ""

toPair : a -> List a -> (a, a)
toPair d list = 
  case list of
    [one, two] -> (one, two)
    one::two::_ -> (one, two)
    [one] -> (one, d)
    _ -> (d, d)