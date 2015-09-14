module Model where

import List exposing (map, foldr, sum, length)

type alias Student =
  { name  : String
  , score : Int }

clampScore : Student -> Student
clampScore {name, score} = Student name <| clamp 0 100 score

empty : Student
empty = Student "" 0

type Input
  = Update (Int, Student)
  | Delete Int
  | Create
  | Empty

type alias Model =
  List Student

type alias Metrics =
  (Int, Int, Int)

initial : Model
initial = [empty]

metrics : Model -> Metrics
metrics students = case students of
  [] -> (0, 0, 0)
  _  -> let
    x // y = round <| toFloat x / toFloat y
    scores = map .score students
    min'   = foldr min 100   scores
    max'   = foldr max 0     scores
    avg'   = sum scores // length scores
    in (min', max', avg')
