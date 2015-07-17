module Main where

import Html            exposing (Html, div)
import Html.Attributes exposing (class)
import Signal          exposing (..)
import Task            exposing (Task)
import Debug

import Model           exposing (..)
import View            exposing (view, createKey)
import Controller      exposing (control)

import Test

port run : Signal (Task x ())
port run = createKey

main : Signal Html
main = let
  render state = div [ class "ui text container" ]
    (view (Debug.watch "state" state) ++ Test.test)
  in render <~ foldp control initial (.signal input)
