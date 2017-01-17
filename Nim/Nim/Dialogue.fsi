module Nim.Dialogue

open Nim.AsyncEventQueue
open Nim.Game.Board

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

type OutputMessage =
    | CreateGame of board
    | Update of board 
    | GameOver of string 
    | Error of string 
    | Downloading

val evIn : AsyncEventQueue<InputMessage>
val evOut : AsyncEventQueue<OutputMessage>