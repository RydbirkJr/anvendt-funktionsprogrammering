module Nim.Gui

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 
open Nim.Dialogue
open Nim.Game.Board

// Game elements
let mutable heapBoxes: NumericUpDown list = []
let mutable currentBoard = Nim.Game.Board.make 1 1 


let parseTextBox (box:NumericUpDown) =
    int box.Value

let postMove m = 
    match m with 
    | Some (i, c) -> evIn.Post(Move (i, c))
    | _ -> MessageBox.Show( "Please make a change") |> ignore

let window =
  new Form(Text="Nim", Size=Size(850,225))

let newGameButton =
  new Button(Location=Point(350,120),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="New game")

let webGameButton =
    new Button(Location=Point(150,120),MinimumSize=Size(100,50),
        MaximumSize=Size(100,50),Text="Download game")

let cancelButton =
    new Button(Location=Point(250,120),MinimumSize=Size(100,50),
        MaximumSize=Size(100,50),Text="Cancel download")

let disable bs = 
    for b in [newGameButton;webGameButton;cancelButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

let numberOfHeapsLabel =
    new Label(Location=Point(50, 50),Text="Number of heaps:")

let numberOfHeapsInput = 
  new NumericUpDown(Location=Point(160 ,50),Size=Size(40,25), Maximum = decimal 10,Minimum = decimal 2, Value = decimal 10) 

let numberOfMatchesLabel =
    new Label(Location=Point(50, 80),Text="Max number of matches pr. heap:")

let numberOfMatchesInput = 
  new NumericUpDown(Location=Point(160 ,80),Size=Size(40,25),Maximum = decimal 99,Minimum = decimal 2, Value = decimal 10)

//Disable
let moveButton =
  new Button(Location=Point(350,120),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Move")

let forfeitButton = 
    new Button(Location=Point(250,120),MinimumSize=Size(100,50),
            MaximumSize=Size(100,50),Text="Give up")

let newGame() =
    let heaps = int numberOfHeapsInput.Text
    let matchesMax = int numberOfMatchesInput.Text
    disable[newGameButton;webGameButton;cancelButton]
    evIn.Post (NewGame (heaps, matchesMax))

let cancel() =
    disable[cancelButton]
    evIn.Post(Cancel)

let move() =
    Game.Board.map (fun x -> Nim.Game.Heap.matches x) currentBoard 
    |> List.mapi2(fun i x y -> (i, parseTextBox(x),y)) heapBoxes
    |> List.filter(fun (i,y,x)-> y <> x)
    |> List.map(fun (i,x,y) -> (i, x))
    |> List.tryHead
    |> postMove

let showWelcomeGUI() =
    window.Controls.Add newGameButton
    window.Controls.Add numberOfHeapsInput
    window.Controls.Add numberOfHeapsLabel
    window.Controls.Add numberOfMatchesInput
    window.Controls.Add numberOfMatchesLabel
    window.Controls.Add webGameButton
    window.Controls.Add cancelButton
    disable[cancelButton]


let listMaker length =
    [1..length]

let makeHeapBoxes board =
    Game.Board.map (fun x -> Nim.Game.Heap.matches x) board 
    |> List.length 
    |> listMaker 
    |> List.map(fun n -> new NumericUpDown(Location=Point(70 * (n) ,25),Size=Size(70,25),Font=new Font("Courier New", float32 28)))
    //|> List.map(fun n -> setFormattingOnInput(n))

// Helper function to update the text boxes with new values
let rec welcome() =
    async {
        let! msg = evOut.Receive()
        match msg with 
            | CreateGame b -> return! createGame b
            | Downloading -> return! downloading()
            | Error err -> printf "%s" err
            | _ -> failwith("Unsupported message in GUI.welcome()")
        }
    
and ready() = 
    async {
        let! msg = evOut.Receive()
        match msg with 
            | Update b -> return! update b
            | GameOver msg ->
                MessageBox.Show(msg) |> ignore
                window.Controls.Remove moveButton
                window.Controls.Remove forfeitButton
                showWelcomeGUI()
                List.iter (fun b -> window.Controls.Remove b) heapBoxes
                heapBoxes <- []
                return! welcome()
            | Error err -> printf "%s" err
            | _ -> failwith("Unsupported message in GUI.ready()")
            }

and createGame board =
    async{
        //Remove welcome view
        disable[]
        window.Controls.Remove newGameButton
        window.Controls.Remove webGameButton
        window.Controls.Remove cancelButton 
        window.Controls.Remove numberOfHeapsInput
        window.Controls.Remove numberOfMatchesInput
        window.Controls.Remove numberOfMatchesLabel
        window.Controls.Remove numberOfHeapsLabel

        //Add buttons for game
        window.Controls.Add moveButton
        window.Controls.Add forfeitButton

        //Add heap boxes
        heapBoxes <- makeHeapBoxes board
        heapBoxes
        |> List.iter (fun b -> window.Controls.Add b)

        return! update(board)

    }

and update board = 
    async {
        currentBoard <- board
        updateHeapBoxes(heapBoxes, Game.Board.map (fun x -> Nim.Game.Heap.matches x) board)
        return! ready()
    }

and updateHeapBoxes(boxes: NumericUpDown list, heaps: int list) = 
    match boxes, heaps with
        |[],[] -> ()
        | b::bt, h::ht -> 
            b.Text <- string h
            b.Maximum <-decimal h
            updateHeapBoxes(bt, ht)
        | _ -> ()

and downloading() =
    async{
      disable[newGameButton;webGameButton]
      let! msg = evOut.Receive()
      match msg with
      | CreateGame b -> return! createGame b
      | Error s -> 
            disable [cancelButton]
            MessageBox.Show(s) |> ignore
            return! welcome() 
      | _ -> failwith("Unexpected message in GUI.downloading()")

    }

showWelcomeGUI()
newGameButton.Click.Add (fun _ -> newGame())
moveButton.Click.Add (fun _ -> move())
webGameButton.Click.Add (fun _ -> evIn.Post(Download))
cancelButton.Click.Add (fun _ -> cancel())
forfeitButton.Click.Add( fun _ -> evIn.Post Forfeit)

// Start
Async.StartImmediate (welcome())