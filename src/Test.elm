module Test where

import Shrink
import List
import Html as H
import Html.Attributes as A
import Random.Extra as Random
import Debug

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
  claim "Create increments length"
    `that` (control Create >> List.length)
    `is` (List.length >> ((+) 1)) `for` state

deleteDecreasesLength : Claim
deleteDecreasesLength = let
  noLess x = if x < 0 then 0 else x
  delete = control (Delete 0) >> List.length >> noLess
  oneLess = List.length >> (flip (-) 1) >> noLess
  in claim "Delete decrements length"
    `that` delete `is` oneLess `for` state

deleteRemovesItemAtIndex : Claim
deleteRemovesItemAtIndex = let
  sanitize (i, s) = (clamp 0 (List.length s - 1) i, s)
  match i i' x = if i == i' then Nothing else Just x
  proof = (<<) (List.filterMap identity) << List.indexedMap << match
  in claim "Delete removes item at index"
  `that` (uncurry (control << Delete) << sanitize)
  `is`   (uncurry proof << sanitize)
  `for` tuple (int, state)

updateClampsScore : Claim
updateClampsScore = let
  isClamped {score} = if | score < 0   -> False
                         | score > 100 -> False
                         | otherwise   -> True

  clean             = List.map << setScore <| clamp 0 100
  exaggerate        = setScore << (*) <| 1000000

  proof (i, s') =
    List.all isClamped
    << control (Update (i, exaggerate s')) << clean

  in claim "Score is clamped"
    `that` uncurry proof
    `is` always True
    `for` tuple (tuple (int, student), state)

updateChangesItemAtIndex : Claim
updateChangesItemAtIndex = let
  proof (at, with) state = case state of
    [] -> True
    _ -> let
      effected = control (Update (at, with)) state
      changed i' s' = if at == i'
        then s' == setScore (clamp 0 100) with else True
      in List.all identity (List.indexedMap changed effected)
  in claim "Update changes item at index, or no op"
  `that` uncurry proof
  `is` always True
  `for` tuple (tuple (int, student), state)

checkControl = suite "Controller"
  [ createAndDelete
  , emptyDoesNothing
  , createIncreasesLength
  , deleteDecreasesLength
  , deleteRemovesItemAtIndex
  , updateChangesItemAtIndex
  , updateClampsScore ]

getMin (min, _, _) = min
getMax (_, max, _) = max
getAvg (_, _, avg) = avg

minIsLowest = let
  lowest s =
    case List.minimum <| List.map .score s of
      Just x  -> if x > 100 then 100 else x
      Nothing -> 0
  in claim "Min is the lowest score or zero,
            and never greater than 100"
    `that` (getMin << metrics)
    `is` lowest
    `for` state

maxIsHighest = let
  highest s =
    case List.maximum <| List.map .score s of
      Just x  -> if x < 0 then 0 else x
      Nothing -> 0
  in claim "Max is the heighest score or zero,
            and never less than zero"
    `that` (getMax << metrics)
    `is` highest
    `for` state

avgIsTheAverage = let
  fancyAvg s = let
    n = toFloat <| List.length s
    t = List.foldl (+) 0 s
    in (1 / n) * t
  sanitizeNaN x = if isNaN x then 0 else x
  proof = round << sanitizeNaN
                << fancyAvg
                << List.map .score
  in claim "Average is average"
    `that` (getAvg << metrics)
    `is` proof
    `for` state

averageIsBetweenMinAndMax = let
  between (min, max, avg) = min <= avg && avg <= max
                         && avg >= min && min <= max
                         && avg <= max && max >= min
  in claim "Average is in the middle, max is biggest, min is smallest"
    `that` (between << metrics)
    `is` always True
    `for` state

checkMetrics = suite "Metrics"
  [ minIsLowest
  , maxIsHighest
  , avgIsTheAverage
  , averageIsBetweenMinAndMax ]

pure x = [x]

test : List H.Html
test = let
  details =
    pure << H.details [ A.style [ ("overflow", "hidden") ] ]
         << (::) (H.summary [] [ H.text "Tests" ])
         << pure << display << quickCheck << suite "Students"
  in details
    [ checkControl
    , checkMetrics ]
