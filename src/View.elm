module View where

import Html            exposing (Html, node, table, text, button, label, div)
import Html.Events     exposing (onClick, on, targetValue)
import Html.Shorthand  exposing (thead_, tbody_, tr_, th_, td_, div_, br')
import Html.Attributes exposing (href, rel, class, type', value)
import Signal          exposing (Address)
import List

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
     , semantic "input" ]

studentTable : Students -> Html
studentTable students = let
  row : Student -> Html
  row student = tr_
    [ th_ [ text           student.name   ]
    , th_ [ text (toString student.score) ]
    , th_
      [ button [ class "ui button"
               , onClick input.address (Delete student) ]
        [ text "Delete" ] ] ]

  title : Html
  title = tr_
    [ th_ [ text "Name"  ]
    , th_ [ text "Grade" ]
    , th_ [ text "Tools" ] ]

  in
  table [ class "ui celled table" ]
    [ thead_ [ title ]
    , tbody_ (List.map row students) ]

new : String -> String -> Html
new name score = let
  onInput a = on "input" targetValue (Signal.message a)
  in
  div [ class "ui form" ]
  [ label [] [ text "Name" ]
  , Html.input [ onInput newName.address
               , value name ] []
  , br'
  , label [] [ text "Score" ]
  , Html.input [ onInput newScore.address
               , type' "number"
               , value score ] []
  , br'
  , button [ onClick submit.address () ]
    [ text "Add Student" ] ]

view : Students -> String -> String -> List Html
view students name score =
  [ studentTable students
  , new name score ]
