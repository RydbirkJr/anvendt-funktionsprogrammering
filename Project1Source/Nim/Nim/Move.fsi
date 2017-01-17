module Nim.Game.Move

open Nim.Game.Board

val optimalMove : board -> (int * int)

val randomMove : board -> (int * int)