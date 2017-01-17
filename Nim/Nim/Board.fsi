module Nim.Game.Board

open Nim.Game.Heap

type board

val move : index:int -> count:int -> board:board -> Choice<string,board>

val make : int -> int -> board

val item : int -> board -> heap

val isGameOver : board -> bool

val numberOfHeaps: board -> int

val numberOfMatches: board -> int

val makeWeb: string -> board

val fold: ('a -> heap -> 'a) -> i:'a -> board -> 'a

val map: (heap -> 'a) -> board -> 'a list