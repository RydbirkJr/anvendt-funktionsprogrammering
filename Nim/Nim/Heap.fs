module Nim.Game.Heap

type heap = H of int

let gt (H(c)) (H(n)) = n > c
let lt (H(c)) (H(n)) = n < c
let eq c (H(n)) = n = c
let make matches = H(matches)
let matches (H(matches)) = matches
let xorb (H(i)) (H(j)) = H(i ^^^ j)