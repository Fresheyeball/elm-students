module Main where

import Html            exposing (Html, div)
import Html.Attributes exposing (class)
import Signal          exposing (..)
import Task            exposing (Task)
import Effects         exposing (Never, Effects)
import Debug

import Model           exposing (..)
import View
import Controller      exposing (control)
import StartApp        exposing (App, Config)

-- import Test

view : Address Input -> State -> Html
view address state = div [ class "ui text container" ]
  (View.view address (Debug.watch "state" state) )

app : App State
app =
  StartApp.start
    { init   = (initial, Effects.none)
    , update = control
    , view   = view
    , inputs = [] }

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

main : Signal Html
main =
  app.html
