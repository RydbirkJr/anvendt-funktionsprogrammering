module Nim.Dialogue

open System
open System.Net
open System.Threading 
open Nim.AsyncEventQueue
open Nim.Game.Board
open Nim.Game.Move
open Nim.Game.Ai

// Inbound messages sent from gui
type InputMessage = 
    | NewGame of int * int 
    | Move of int * int 
    | Download 
    | DownloadComplete of string 
    | Cancel 
    | Cancelled 
    | DownloadError 
    | Done of Choice<string,board>
    | Forfeit

let evIn = AsyncEventQueue<InputMessage>()

// Outbound messages to gui
type OutputMessage = 
    | CreateGame of board
    | Update of board 
    | GameOver of string 
    | Error of string
    | Downloading

let evOut = AsyncEventQueue<OutputMessage>()

let mutable board = Nim.Game.Board.make 0 0
let mutable difficulty = 1

// Game automaton
let rec welcome() = 
    async {
         let! msg = evIn.Receive()
         match msg with
         | NewGame (n,m) -> return! newGame n m
         | Download -> return! loading()
         | _ -> failwith("Unacceptable message")
    }

and ready() = 
    async {
         let! msg = evIn.Receive()
         match msg with
         | Move (i,c) -> return! move i c
         | Forfeit ->
             evOut.Post (GameOver "You gave up!")
             return! welcome()
         | _ -> failwith("Unacceptable message")
    }

and newGame numberOfHeaps maximumNumberOfMatches = 
    async {
        board <- Nim.Game.Board.make numberOfHeaps maximumNumberOfMatches
        evOut.Post (CreateGame board)
        return! ready()
    }

and loading() =
    async {
        evOut.Post(Downloading)
        let url = "http://nim.getroomi.com/"
        use ts = new CancellationTokenSource()

        Async.StartWithContinuations
            (async{
                let webClient = new WebClient()
                let! data = webClient.AsyncDownloadString(Uri url)
                return data },
                (fun data -> evIn.Post(DownloadComplete data)),
                (fun _ -> evIn.Post DownloadError),
                (fun _ -> evIn.Post Cancelled),
                ts.Token
                )
        let! msg = evIn.Receive()
        match msg with
            | DownloadComplete data -> 
                let b = Game.Board.makeWeb(data)
                board <- b
                evOut.Post (CreateGame board)
                return! ready()
            | Cancel -> 
                ts.Cancel()
                return! cancelling()
            | DownloadError -> 
                evOut.Post (Error "Download failed.")
                return! welcome()
            | _ -> failwith("Unknown error.") 
    }

and cancelling() =
    async{
        let! msg = evIn.Receive()
        match msg with
            | Cancelled | DownloadError | DownloadComplete _ -> 
                evOut.Post(Error "Download cancelled")
                return! welcome() 
            | _ -> failwith("Cancelling: Unexpected message")
    }

and move index count = 
    async {
        let b = Nim.Game.Board.move index count board 
        match b with
        | Choice1Of2 err -> 
            evOut.Post (Error err)
        | Choice2Of2 b when Nim.Game.Board.isGameOver b -> 
            board <- b
            evOut.Post (Update b)
            evOut.Post (GameOver "You won!")
            return! welcome()
        | Choice2Of2 b ->
            board <- b
            evOut.Post (Update b)
            return! ai()
    }
     
and ai() =
    async {
        let (index, count) = Nim.Game.Ai.move difficulty board
        let b = Nim.Game.Board.move index count board 
        match b with 
        | Choice2Of2 b when Nim.Game.Board.isGameOver b -> 
            board <- b
            evOut.Post (Update b)
            evOut.Post (GameOver "AI won!")
            return! welcome()
        | Choice2Of2 b ->
            board <- b
            evOut.Post (Update b)
            return! ready()
        | Choice1Of2 err -> 
            failwith(err)
    }

// Start
Async.StartImmediate (welcome())