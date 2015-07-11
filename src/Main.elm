module Main where

import Html            exposing (Html)
import Html.Shorthand  exposing (div_)
import Signal          exposing (..)

import Model           exposing (input, dummy, newName, newScore)
import View            exposing (semantic'ui, view)
import Controller      exposing (control, new)

main : Signal Html
main = let
  render s n s' = div_ <| semantic'ui ++ view s n s'
  in render <~ foldp control dummy (new `merge` input.signal)
             ~ newName.signal
             ~ newScore.signal
