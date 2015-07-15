module Controller where

import Result      exposing (..)
import Signal      exposing (..)
import Debug

import Model       exposing (..)

control : Input -> State -> State
control input state = let
  (**) : Int -> List Student -> State
  (**) i x = { state | list <-
    List.take  i      (.list state) ++ x ++
    List.drop (i + 1) (.list state) }
  in case Debug.watch "input" input of
    Create            ->
      { list      = .list state ++ [ empty ]
      , valid     = "" }
    Update (from, to) ->
      from ** [ { to | score <- to |> .score >> clamp 0 100 } ]
    Delete corpse     -> corpse ** []
    Invalid error     -> { state | valid <- error }
    Empty             -> state
