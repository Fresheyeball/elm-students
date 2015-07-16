module Controller where

import Result      exposing (..)
import Signal      exposing (..)
import Debug

import Model       exposing (..)

control : Input -> State -> State
control input state = let
  updateAt : Int -> State-> State
  updateAt i x =
    List.take i state ++ x ++ List.drop (i + 1) state
  in case Debug.watch "input" input of
    Create            -> state ++ [ empty ]
    Update (from, to) ->
      updateAt from [ { to | score <- to |> .score >> clamp 0 100 } ]
    Delete corpse     -> updateAt corpse []
    Empty             -> state
