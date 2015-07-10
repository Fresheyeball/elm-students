module Main where

import Html            exposing (Html)
import Html.Shorthand  exposing (div_)
import Signal          exposing (..)

import Model           exposing (..)
import View            exposing (..)
import Controller      exposing (..)

main : Signal Html
main = let
  render s = div_ <| semantic'ui ++ view s
  in render <~ foldp control dummy input.signal
