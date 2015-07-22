module Controller where

import Result      exposing (..)
import Signal      exposing (..)
import List        exposing (take, drop)
import Debug

import Model       exposing (..)

control : Input -> State -> State
control input state = let
  updateAt : Int -> State-> State
  updateAt i x =
    take i state ++ x ++ drop (i + 1) state
  in case Debug.watch "input" input of
    Create            -> state ++ [ empty ]
    Update (from, to) ->
      updateAt from [ clampScore to ]
    Delete corpse     -> updateAt corpse []
    Empty             -> state
