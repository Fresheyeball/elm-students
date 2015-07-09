module Main where

import Html            exposing (Html)
import Html.Shorthand  exposing (div_)
import Signal          exposing (..)

import Model           exposing (..)
import View            exposing (..)
import Controller      exposing (..)

input : Mailbox Input
input =
  mailbox Empty

students : Signal Students
students =
  foldp control dummy input.signal

main : Signal Html
main = let
  render s = div_ <| semantic'ui ++ [ view s input.address ]
  in render <~ students
