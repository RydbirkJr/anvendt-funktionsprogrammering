module Nim.Game.Move

open Nim.Game.Board
open System

let m board = 
    board 
    |> Board.fold (fun a b -> Heap.xorb a b) (Heap.make 0)
    |> Heap.matches;;

let findIndex m board = 
    board 
    |> Board.map(fun elem -> (Heap.xorb m elem) |> Heap.lt elem)
    |> List.findIndex (fun x -> x) ;;
   
let largestIndex board = 
    let maxVal = (board |> Board.map (fun x -> Heap.matches x) |> List.max)
    board |> Board.map (fun x -> Heap.matches x) |> List.findIndex (fun x -> x = maxVal);;

let optimalMove board = 
    match m board with 
    | 0 -> 
        let i = largestIndex board
        let c = (Board.item i board |> Heap.matches) - 1
        (i, c)
    | n -> 
        let i = findIndex (Heap.make n) board
        let c = (Board.item i board |> Heap.matches)
        (findIndex (Heap.make n) board, c ^^^ n)

let randomMove board = 
    let i = largestIndex board
    let c = System.Random().Next(Heap.matches (Board.item i board))
    (i, c)