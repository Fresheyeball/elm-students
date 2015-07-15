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
  | Invalid String
  | Delete Int
  | Create
  | Empty

type alias State =
  { list     : List Student
  , valid    : String }

dummy : State
dummy =
  { list  = [ { name = "", score = 0 } ]
  , valid = "" }

input : Mailbox Input
input =
  mailbox Empty

getMetrics : List Student -> (Int, Int, Int)
getMetrics students = case students of
  [] -> (0, 0, 0)
  _  -> let
    scores = List.map .score students
    min'   = List.foldr min 100   scores
    max'   = List.foldr max 0     scores
    avg'   = toFloat (List.sum    scores)
           / toFloat (List.length scores)
           |> round
    in (min', max', avg')
