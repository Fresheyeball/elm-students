module Model where

import Signal exposing (Mailbox, mailbox)
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

type alias State =
  List Student

initial : State
initial = [empty]

metrics : State -> (Int, Int, Int)
metrics students = case students of
  [] -> (0, 0, 0)
  _  -> let
    scores = map .score students
    min'   = foldr min 100 scores
    max'   = foldr max 0   scores
    avg'   = sum scores // length scores
    in (min', max', avg')
