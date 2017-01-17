module Nim.Game.Heap

type heap

val gt : heap -> heap -> bool // Checks is a heap is greater than an integer
val lt : heap -> heap -> bool // Checks is a heap is less than an integer
val eq : int -> heap -> bool // Checks is a heap is equal to an integer
val make : int -> heap // Make heap
val matches : heap -> int
val xorb : heap -> heap -> heap