module Update where

import Result      exposing (..)
import Signal      exposing (..)
import List        exposing (take, drop)
import Debug
import Effects     exposing (Effects)

import Model       exposing (..)

update : Input -> State -> (State, Effects Input)
update input state = let
  updateAt : Int -> State -> State
  updateAt i x =
    take i state ++ x ++ drop (i + 1) state
  newState : State
  newState = case input of
    Create            -> state ++ [ empty ]
    Update (from, to) ->
      updateAt from [ clampScore to ]
    Delete corpse     -> updateAt corpse []
    Empty             -> state
  in (newState, Effects.none)
