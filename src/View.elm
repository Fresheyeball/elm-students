module View where

import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Shorthand  exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (Address)
import List.Extra      exposing (dropWhile)
import Json.Decode     exposing (decodeString, int, customDecoder)
import List
import String

import Model exposing (..)

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

studentTable : Address Input -> List Student -> Html
studentTable address students = let
  field :  (Student -> String)
        -> (String  -> Student)
        -> Int -> Student -> String -> Html
  field l f index student placeholder' = let
    message s =
      Signal.message
      address
      (Update (index, f s) )
    in div [ class "ui big input fluid" ]
      [ Html.input [ on "input" targetValue message
                   , onEnter address Create
                   , autofocus (index == List.length students - 1)
                   , placeholder placeholder'
                   , value (l student) ] [] ]

  onEnter : Address a -> a -> Attribute
  onEnter address value = let
    is13 code =
      if code == 13 then Ok () else Err "not the right key code"
    in on "keydown"
      (customDecoder keyCode is13)
      (\_ -> Signal.message address value)

  name : Int -> Student -> Html
  name index student =
    field .name (\s -> { student | name <- s }) index student "Enter student name"

  score : Int -> Student -> Html
  score index student = let
    g s = { student | score <-
      let s' = s
        |> String.toList
        >> dropWhile ((==) '0')
        >> String.fromList
      in case decodeString int s' of
        Ok i  -> i
        Err _ -> 0 }
    in field (toString << .score) g index student "Enter student score"

  failing : Student -> List Html.Attribute
  failing {score, name} =
    if | name /= "" && score < 70 -> [ class "negative" ]
       | score >= 90              -> [ class "positive" ]
       | otherwise                -> []

  row : Int -> Student -> Html
  row index student = tr (failing student)
    [ td_ [ name  index student ]
    , td [ class "collapsing" ]
      [ score index student ]
    , td [ class "collapsing" ]
      [ button [ class "ui button tiny"
               , onClick address (Delete index) ]
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
    , tfoot_ [ tr_ [ th [ colspan 3 ]
                     [ create address, minMaxAvg students ] ] ] ]

minMaxAvg : Model -> Html
minMaxAvg students = let
  (min', max', avg') = metrics students

  style' k v = style [(k, v)]

  label s x =
    div [ class "ui label"
        , style' "width" "60px" ]
    [ text s
    , div [ class "detail" ]
      [ text <| toString x ] ]

  in div [ style' "float" "right" ]
    [ label "Min" min'
    , label "Max" max'
    , label "Avg" avg' ]

create : Address Input -> Html
create address =
  button [ class "ui button primary tiny"
         , onClick address Create ]
  [ text "Add Student" ]

view : Address Input -> Model -> List Html
view address model =
  semantic'ui ++ [ studentTable address model ]
