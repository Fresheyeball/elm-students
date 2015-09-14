module Update where

import List        exposing (take, drop)
import Effects     exposing (Effects)

import Model       exposing (..)

update : Input -> Model -> (Model, Effects Input)
update input model = let
  updateAt : Int -> Model -> Model
  updateAt i x =
    take i model ++ x ++ drop (i + 1) model
  newModel : Model
  newModel = case input of
    Create            -> model ++ [ empty ]
    Update (from, to) ->
      updateAt from [ clampScore to ]
    Delete corpse     -> updateAt corpse []
    Empty             -> model
  in (newModel, Effects.none)
