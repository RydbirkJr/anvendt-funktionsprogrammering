module Nim.Game.Board

open System
open Nim.Game.Heap

type board = B of heap list

let rand = Random()

let move index count (B(heaps)) =
    match count with 
    | count when count < 0 -> 
        Choice1Of2 "No negative numbers!"
    | count when (List.item index heaps) |> Heap.lt (Heap.make(count)) -> 
        Choice1Of2 "Don't try increment the number of matches!"
    | count when (List.item index heaps) |> Heap.eq count -> 
        Choice1Of2 "Please make a change!!"
    | _ when (List.length heaps) - 1 < index ->
        failwith("Index out of bounds")
    | _ -> 
        heaps
        |> List.mapi (fun i e -> if i = index then Heap.make count else e)
        |> B 
        |> Choice2Of2

let iter fn (B(heaps)) =
    heaps 
    |> List.iter fn

let fold fn i (B(heaps)) =
    heaps |>
    List.fold fn i;;

let map fn (B(heaps)) =
    heaps 
    |> List.map fn

let make numberOfHeaps maximumNumberOfMatches = 
    let n = Math.Max(numberOfHeaps, 2)
    [1..n] 
    |> List.map(fun _ -> Math.Max(rand.Next(maximumNumberOfMatches), 1))
    |> List.map Heap.make
    |> B

let item index (B(heaps)) =
    List.item index heaps

let isGameOver (B(h)) = 
    List.map(fun h -> Heap.matches(h)) h
    |> List.sum = 0

let numberOfHeaps (B(h)) = 
    List.map(fun h -> Heap.matches(h)) h
    |> List.length
    
let numberOfMatches (B(h)) = 
    List.map(fun h -> Heap.matches(h)) h 
    |> List.sum

let makeWeb (s: string) =
    s.Split(',') 
    |> Seq.map Int32.Parse 
    |> Seq.map Heap.make 
    |> List.ofSeq 
    |>  B