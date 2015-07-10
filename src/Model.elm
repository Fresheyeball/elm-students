module Model where

import Signal exposing (Mailbox, mailbox)

type alias Student =
  { name  : String
  , score : Int }

type alias Students =
  List Student

type Input
  = Create Student
  | Update (Student, Student)
  | Delete Student
  | Empty

dummy : Students
dummy =
  [ { name = "Jack", score = 96 }
  , { name = "Jill", score = 63 } ]

input : Mailbox Input
input =
  mailbox Empty

newName : Mailbox String
newName =
  mailbox ""

newScore : Mailbox String
newScore =
  mailbox ""

submit : Mailbox ()
submit =
  mailbox ()
