// Learn more about F# at http://fsharp.org

open System
open Othello

type Player =
    { Color : Board.Color
      Move : Board.State -> int * int }
    
let rec gameLoop board (player:Player) (otherPlayer:Player) =
    if (Seq.isEmpty (Board.getValidMoves player.Color board)) then
        Console.showGameResults board
    else
        begin
            System.Console.WriteLine()
            Console.print board
            let move = player.Move board
            let newBoard = Board.move move player.Color board
            gameLoop newBoard otherPlayer player
        end

let rec createPlaryer color =
    printfn "Choose player type for %c" (Console.colorToChar (Some color))
    printfn "1: Human"
    printfn "2: Computer - Random"
    printfn "3: Computer - Greedy"
    printfn ""
    
    match (Int32.TryParse (Console.ReadLine().Trim())) with
    | (true, 1) -> {Color = color; Move = Console.getUserInput color}
    | (true, 2) -> {Color = color; Move = AI.random() color}
    | (true, 3) -> {Color = color; Move = AI.greedy(Scoring.scoreDifference) color}
    | _ -> createPlaryer color

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let board = Board.create()
    let player1 = createPlaryer Board.White
    let player2 = createPlaryer Board.Black
    gameLoop board player1 player2 
    0 // return an integer exit code
