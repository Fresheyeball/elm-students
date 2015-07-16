module Model where

import Signal exposing (Mailbox, mailbox)

type alias Student =
  { name  : String
  , score : Int }

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
    scores = List.map .score students
    min'   = List.foldr min 100   scores
    max'   = List.foldr max 0     scores
    avg'   = toFloat (List.sum    scores)
           / toFloat (List.length scores)
           |> round
    in (min', max', avg')
