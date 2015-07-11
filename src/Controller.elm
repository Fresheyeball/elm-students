module Controller where

import Json.Decode exposing (..)
import Result      exposing (..)
import Signal      exposing (..)

import Model       exposing (..)

new : Signal Input
new = let
  g name score =
    case decodeString int score of
      Ok score' -> Create { name = name, score = score' }
      _         -> Empty
  in g <~ newName.signal ~ newScore.signal
     |> sampleOn submit.signal

control : Input -> Students -> Students
control input target =
  case input of
    Create baby       -> baby::target
    Update (from, to) -> to::control (Delete from) target
    Delete corpse     -> List.filter ((/=) corpse) target
    Empty             -> target
