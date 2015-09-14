module Test where

import Shrink
import List
import Html as H
import Html.Attributes as A
import Random.Extra as Random
import Effects exposing (Effects)

import Model exposing (..)
import Update exposing (..)

import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (display)

student : Investigator Student
student = let
  shrinker { name, score } =
    Student `Shrink.map`    shrink string name
            `Shrink.andMap` shrink int score
  generator =
    Student `Random.map`    random string
            `Random.andMap` random int
  in investigator generator shrinker

model : Investigator Model
model = list student

createAndDelete : Claim
createAndDelete = let
  f : Model -> (Model, Effects Input)
  f model =
    update (Delete (List.length model)) (fst <| update Create model)
  in
  claim "Delete on last element, should reset Create"
    `that` (fst << f) `is` identity `for` model

emptyDoesNothing : Claim
emptyDoesNothing =
  claim "Empty input does nothing"
    `that` (fst << update Empty) `is` identity `for` model

createIncreasesLength : Claim
createIncreasesLength =
  claim "Create increments length"
    `that` (update Create >> fst >> List.length)
    `is` (List.length >> (+) 1) `for` model

deleteDecreasesLength : Claim
deleteDecreasesLength = let
  noLess x = if x < 0 then 0 else x
  delete = update (Delete 0) >> fst >> List.length >> noLess
  oneLess = List.length >> flip (-) 1 >> noLess
  in claim "Delete decrements length"
    `that` delete `is` oneLess `for` model

deleteRemovesItemAtIndex : Claim
deleteRemovesItemAtIndex = let
  sanitize (i, s) = (clamp 0 (List.length s - 1) i, s)
  match i i' x = if i == i' then Nothing else Just x
  proof = (<<) (List.filterMap identity) << List.indexedMap << match
  in claim "Delete removes item at index"
  `that` (fst << uncurry (update << Delete) << sanitize)
  `is`   (uncurry proof << sanitize)
  `for` tuple (int, model)

updateClampsScore : Claim
updateClampsScore = let
  isClamped {score} = if
    | score < 0   -> False
    | score > 100 -> False
    | otherwise   -> True

  clean = List.map clampScore
  exaggerate {name, score} =
    Student name (score * 1000000)

  proof (i, s') =
    List.all isClamped
    << fst << update (Update (i, exaggerate s')) << clean

  in claim "Score is clamped"
    `that` uncurry proof
    `is` always True
    `for` tuple (tuple (int, student), model)

updateChangesItemAtIndex : Claim
updateChangesItemAtIndex = let
  proof (at, with) model = case model of
    [] -> True
    _  -> let
      (effected, _) = update (Update (at, with)) model
      changed i' s' = if at == i'
        then s' == clampScore with else True
      in List.all identity (List.indexedMap changed effected)
  in claim "Update changes item at index, or no op"
  `that` uncurry proof
  `is` always True
  `for` tuple (tuple (int, student), model)

checkupdate : Claim
checkupdate = suite "Update"
  [ createAndDelete
  , emptyDoesNothing
  , createIncreasesLength
  , deleteDecreasesLength
  , deleteRemovesItemAtIndex
  , updateChangesItemAtIndex
  , updateClampsScore ]

getMin : Metrics -> Int
getMin (min, _, _) = min
getMax : Metrics -> Int
getMax (_, max, _) = max
getAvg : Metrics -> Int
getAvg (_, _, avg) = avg

minIsLowest : Claim
minIsLowest = let
  lowest s =
    case List.minimum <| List.map .score s of
      Just x  -> if x > 100 then 100 else x
      Nothing -> 0
  in claim "Min is the lowest score or zero,
            and never greater than 100"
    `that` (getMin << metrics)
    `is` lowest
    `for` model

maxIsHighest : Claim
maxIsHighest = let
  highest s =
    case List.maximum <| List.map .score s of
      Just x  -> if x < 0 then 0 else x
      Nothing -> 0
  in claim "Max is the heighest score or zero,
            and never less than zero"
    `that` (getMax << metrics)
    `is` highest
    `for` model

avgIsTheAverage : Claim
avgIsTheAverage = let
  fancyAvg : List Int -> Float
  fancyAvg s = let
    n = toFloat <| List.length s
    t = List.foldl (+) 0 s
    in (1 / n) * t
  sanitizeNaN x = if isNaN x then 0 else x
  proof : Model -> Int
  proof = round << sanitizeNaN
                << fancyAvg
                << List.map .score
  in claim "Average is average"
    `that` (getAvg << metrics)
    `is` proof `for` model

averageIsBetweenMinAndMax : Claim
averageIsBetweenMinAndMax = let
  between (min, max, avg) = min <= avg && avg <= max
                         && avg >= min && min <= max
                         && avg <= max && max >= min
  in claim "Average is in the middle, max is biggest, min is smallest"
    `that` (between << metrics)
    `is` always True
    `for` model

checkMetrics : Claim
checkMetrics = suite "Metrics"
  [ minIsLowest
  , maxIsHighest
  , avgIsTheAverage
  , averageIsBetweenMinAndMax ]

pure : a -> List a
pure x = [x]

test : List H.Html
test = let
  details =
    pure << H.details [ A.style [ ("overflow", "hidden") ] ]
         << (::) (H.summary [] [ H.text "Tests" ])
         << pure << display << quickCheck << suite "Students"
  in details
    [ checkupdate
    , checkMetrics ]
