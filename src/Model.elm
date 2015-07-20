module Model where

import Signal exposing (Mailbox, mailbox)
import List exposing (map, foldr, sum, length)

type alias Student =
  { name  : String
  , score : Int }

setScore : (Int -> Int) -> Student -> Student
setScore f ({score} as student) = { student | score <- f score }

empty : Student
empty =
  { name  = ""
  , score = 0 }

type Input
  = Update (Int, Student)
  | Delete Int
  | Create
  | Empty

type alias State =
  List Student

initial : State
initial = []

input : Mailbox Input
input =
  mailbox Empty

metrics : State -> (Int, Int, Int)
metrics students = case students of
  [] -> (0, 0, 0)
  _  -> let
    scores = map .score students
    min'   = foldr min 100   scores
    max'   = foldr max 0     scores
    avg'   = toFloat (sum    scores)
           / toFloat (length scores)
           |> round
    in (min', max', avg')
