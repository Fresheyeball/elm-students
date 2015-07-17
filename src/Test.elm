module Test where

import Shrink
import Html as H
import Html.Attributes as A
import Random.Extra as Random

import Model       exposing (..)
import Controller  exposing (..)

import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (display)

fromTuple : (Int, String) -> Student
fromTuple (score, name) = { score = score, name = name }
toTuple : Student -> (Int, String)
toTuple {score, name} = (score, name)

student : Investigator Student
student = let
  shrinker { name, score } =
    Student `Shrink.map`    shrink string name
            `Shrink.andMap` shrink int score
  generator =
    Student `Random.map`    random string
            `Random.andMap` random int
  in investigator generator shrinker

state : Investigator State
state = list student

createAndDelete : Claim
createAndDelete = let
  f : State -> State
  f state =
    control (Delete (List.length state)) (control Create state)
  in
  claim "Delete on last element, should reset Create"
    `that` f `is` identity `for` state

emptyDoesNothing : Claim
emptyDoesNothing =
  claim "Empty input does nothing"
    `that` control Empty `is` identity `for` state

createIncreasesLength : Claim
createIncreasesLength =
  claim "Create Increases Length"
    `that` (control Create >> List.length)
    `is` (List.length >> ((+) 1)) `for` state

deleteDecreasesLength : Claim
deleteDecreasesLength = let
  noLess x = if x < 0 then 0 else x
  delete = control (Delete 0) >> List.length >> noLess
  oneLess = List.length >> (flip (-) 1) >> noLess
  in claim "Delete Decreases Length"
    `that` delete `is` oneLess `for` state

pure x = [x]

test : List H.Html
test = let
  details =
    pure << H.details [ A.style [ ("overflow", "hidden") ] ]
         << (::) (H.summary [] [ H.text "Tests" ])
         << pure << display << quickCheck << suite "Controller"
  in details
    [ createAndDelete
    , emptyDoesNothing
    , createIncreasesLength
    , deleteDecreasesLength ]
