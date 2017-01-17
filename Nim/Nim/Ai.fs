module Nim.Game.Ai

open System
open Nim.Game.Move
open Nim.Game.Board

let rand = Random()

let move difficulty board = 
    //c = chance to make a mistake
    let c = float difficulty * float 33 * (Math.Log10 (float (Board.numberOfMatches board) / (float (Board.numberOfHeaps board))))
    match float(rand.Next(100)) with 
    | i when i > c ->
        Move.optimalMove board
    | _ ->
        printf("Whoops!")
        Move.randomMove board