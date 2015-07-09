module View where

import Html            exposing (Html, node, table, text, button, input, label)
import Html.Events     exposing (onClick)
import Html.Shorthand  exposing (thead_, tbody_, tr_, th_, td_, div_)
import Html.Attributes exposing (href, rel, class)
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

view : Students -> Address Input -> Html
view students address = let
  row : Student -> Html
  row student = tr_
    [ th_ [ text           student.name   ]
    , th_ [ text (toString student.score) ]
    , th_
      [ button [ class "ui button"
               , onClick address (Delete student) ]
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

new : Html
new = div_
  [ label [] [ text "Name" ] ]

layers : Students -> Address Input -> List Html
layers students address =
  [ view students address ]
