module Controller where

import Model exposing (..)

control : Input -> Students -> Students
control input target =
  case input of
    Delete corpse -> List.filter ((/=) corpse) target
    Empty -> target
