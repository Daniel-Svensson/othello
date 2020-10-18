// Learn more about F# at http://fsharp.org

open System
open Othello

type Player =
    { Name: string
      Color: Board.Color
      Move: Board.State -> int * int }

let rec gameLoop board (player: Player) (otherPlayer: Player) =
    if (Seq.isEmpty (Board.getValidMoves player.Color board)) then
        if (Seq.isEmpty (Board.getValidMoves otherPlayer.Color board)) then
            Console.showGameResults board
            printfn "%A: %s" (Console.colorToChar (Some player.Color)) player.Name
            printfn "%A: %s" (Console.colorToChar (Some otherPlayer.Color)) otherPlayer.Name
        else
            gameLoop board otherPlayer player
    else
        (System.Console.WriteLine()
         Console.print board

         let move = player.Move board
         let newBoard = Board.move move player.Color board
         gameLoop newBoard otherPlayer player)

type PlayerType =
    { Name: string
      Move: Board.Color -> Board.State -> int * int }

let aiTypes =
    [|
        {Name = "Random"; Move = AI.random()}
        {Name = "Greedy"; Move = AI.greedy (Scoring.scoreDifference)}
        {Name = "Positional"; Move = AI.greedy (Scoring.scorePositional)}
        {Name = "Positional 2"; Move = AI.greedy (Scoring.scorePositional2)}
        {Name = "Movability only"; Move = AI.greedy (Scoring.moveDifference)} 
        {Name = "Combined "; Move = AI.greedy (Scoring.combine 1 Scoring.scorePositional2 3 Scoring.moveDifference)}
    |]

let rec createPlaryer color =
    printfn "Choose player type for %c" (Console.colorToChar (Some color))
    printfn "1: Human"
    for i in [0..aiTypes.Length-1] do
        printfn "%d: Computer - %s" (i+2) aiTypes.[i].Name
    printfn ""

    match (Int32.TryParse(Console.ReadLine().Trim())) with
    | (true, 1) ->
        { Name = color.ToString() 
          Color = color
          Move = Console.getUserInput color }
    | (true, i) when i > 1 && i-2 < aiTypes.Length ->
        let ai = aiTypes.[i-2]
        { Name = ai.Name + " " + color.ToString() 
          Color = color
          Move = ai.Move color }
    | _ -> createPlaryer color

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let board = Board.create ()
    let player1 = createPlaryer Board.White
    let player2 = createPlaryer Board.Black
    gameLoop board player1 player2
    0 // return an integer exit code
