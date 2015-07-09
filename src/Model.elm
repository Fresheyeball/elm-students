module Model where

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
