module View where

import Html            exposing (Html, node, table, text, button, label, div, th, tr, td, form)
import Html.Events     exposing (onClick, on, targetValue)
import Html.Shorthand  exposing (thead_, tbody_, tfoot_, tr_, th_, td_, div_, br')
import Html.Attributes exposing (href, rel, class, type', value, style, colspan, placeholder)
import Signal          exposing (Address)
import List.Extra      exposing (dropWhile)
import Json.Decode     exposing (decodeString, int)
import Task            exposing (Task)
import Keyboard
import List
import String
import Debug

import Model exposing (..)

createKey : Signal (Task x ())
createKey = let
  create : Task x ()
  create = Signal.send (.address input) Create
  g : Bool -> Maybe (Task x ())
  g b = if b then Just create else Nothing
  in Signal.filterMap g create Keyboard.enter

linkCSS : String -> Html
linkCSS url = node "link"
  [ href url
  , rel "stylesheet" ] []

semantic'ui : List Html
semantic'ui = let
  semantic s = linkCSS <|
    "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.0.0/components/"
    ++ s ++ ".css"
  in [ semantic "table"
     , semantic "button"
     , semantic "form"
     , semantic "input"
     , semantic "container"
     , semantic "label" ]

studentTable : List Student -> Html
studentTable students = let
  field :  (Student -> String)
        -> (String  -> Student)
        -> Int -> Student -> String -> Html
  field l f index student placeholder' = let
    message s =
      Signal.message
      (.address input)
      (Update (index, f s) )
    in div [ class "ui input fluid" ]
      [ Html.input [ on "input" targetValue message
                   , placeholder placeholder'
                   , value (l student) ] [] ]

  name : Int -> Student -> Html
  name index student =
    field .name (\s -> { student | name <- s }) index student "Enter student name"

  score : Int -> Student -> Html
  score index student = let
    g s = { student | score <-
      let s' = s |> String.toList >> dropWhile ((==) '0') >> String.fromList
      in case decodeString int s' of
        Ok i  -> i
        Err _ -> 0 }
    in field (toString << .score) g index student ""

  failing : Student -> List Html.Attribute
  failing {score, name} =
    if | name /= "" && score < 70 -> [ class "negative" ]
       | score >= 90              -> [ class "positive" ]
       | otherwise                -> []

  row : Int -> Student -> Html
  row index student = tr (failing student)
    [ td_ [ name  index student ]
    , td_ [ score index student ]
    , td [ class "collapsing" ]
      [ button [ class "ui button"
               , onClick (.address input) (Delete index) ]
        [ text "Delete" ] ] ]

  title : Html
  title = tr_
    [ th_ [ text "Name"  ]
    , th_ [ text "Grade" ]
    , th_ [ text "" ] ]

  in
  table [ class "ui celled table" ]
    [ thead_ [ title ]
    , tbody_ (List.indexedMap row students)
    , tfoot_ [ tr_ [ th [ colspan 3 ] [ create, minMaxAvg students ] ] ] ]

minMaxAvg : List Student -> Html
minMaxAvg students = let
  (min', max', avg') = getMetrics students
  (#>) label x =
    div [ class "ui label" ]
    [ text label
    , div [ class "detail" ]
      [ text <| toString x ] ]
  in div [ style [("float", "right")] ]
    [ "Min" #> min', "Max" #> max', "Avg" #> avg' ]

create : Html
create =
  button [ class "ui button"
         , onClick input.address Create ]
  [ text "Add" ]

view : State -> List Html
view state = semantic'ui ++
  [ studentTable (.list state) ]
