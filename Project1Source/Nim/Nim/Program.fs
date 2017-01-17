// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 
open Nim.Dialogue
open Nim.Gui

[<EntryPoint>]
let main argv = 
    Application.Run(window)

    0 // return an integer exit code

