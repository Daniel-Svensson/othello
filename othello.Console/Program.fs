// Learn more about F# at http://fsharp.org

open System
open Othello

type Player =
    { Name: string
      Color: Board.Color
      Move: Board.State -> int * int }

let rec gameLoop board (player: Player) (otherPlayer: Player) print =
    if (Seq.isEmpty (Board.getValidMoves player.Color board)) then
        if (Seq.isEmpty (Board.getValidMoves otherPlayer.Color board)) then
            Console.showGameResults board
            printfn "%A: %s" (Console.colorToChar (Some player.Color)) player.Name
            printfn "%A: %s" (Console.colorToChar (Some otherPlayer.Color)) otherPlayer.Name
            board
        else
            gameLoop board otherPlayer player print
    else
        (print board

         let move = player.Move board
         let newBoard = Board.move move player.Color board
         gameLoop newBoard otherPlayer player print)

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
        {Name = "Combined"; Move = AI.greedy (Scoring.combine 1 Scoring.scorePositional 2 Scoring.moveDifference)}
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


let evalutateAIs() =
    let wins = System.Collections.Generic.Dictionary<string, int>()
    let score = System.Collections.Generic.Dictionary<string, int>()

    for ai1 in aiTypes do
        wins.Add(ai1.Name, 0)
        score.Add(ai1.Name, 0)
        for ai2 in aiTypes |> Seq.filter (fun x -> x.Name <> ai1.Name) do
            wins.Add(ai1.Name + " vs " + ai2.Name, 0)
            score.Add(ai1.Name + " vs " + ai2.Name, 0)

    for ai1 in aiTypes do
        for ai2 in aiTypes |> Seq.filter (fun x -> x.Name <> ai1.Name) do
            let player1 = { Name = ai1.Name; Color = Board.Color.White; Move = ai1.Move Board.Color.White }
            let player2 = { Name = ai2.Name; Color = Board.Color.Black; Move = ai2.Move Board.Color.Black }
            let result = gameLoop (Board.create()) player1 player2 ignore

            let player1score = Scoring.scoreDifference result player1.Color
            if (player1score > 0) then
                wins.[player1.Name] <- wins.[player1.Name] + 1
                wins.[player1.Name + " vs " + player2.Name] <- wins.[player1.Name + " vs " + player2.Name] + 1
                wins.[player2.Name + " vs " + player1.Name] <- wins.[player2.Name + " vs " + player1.Name] - 1
            else
                wins.[player2.Name] <- wins.[player2.Name] + 1
                wins.[player1.Name + " vs " + player2.Name] <- wins.[player1.Name + " vs " + player2.Name] - 1
                wins.[player2.Name + " vs " + player1.Name] <- wins.[player2.Name + " vs " + player1.Name] + 1

            score.[player1.Name] <- score.[player1.Name] + player1score
            score.[player2.Name] <- score.[player2.Name] - player1score
            score.[player1.Name + " vs " + player2.Name] <- score.[player1.Name + " vs " + player2.Name] + player1score           
            score.[player2.Name + " vs " + player1.Name] <- score.[player2.Name + " vs " + player1.Name] - player1score

    printfn "Wins by value"
    wins 
    |> Seq.sortByDescending (fun x -> x.Value)
    |> Seq.iter (fun x -> printfn "%s : %d" x.Key x.Value)
    
    printfn "\nWins by name"
    wins 
    |> Seq.sortBy (fun x -> x.Key)
    |> Seq.iter (fun x -> printfn "%s : %d" x.Key x.Value)

    printfn "\nScore by name"
    score
    |> Seq.sortBy (fun x -> x.Key)
    |> Seq.iter (fun x -> printfn "%s : %d" x.Key x.Value)

    printfn "\nScore by Value"
    score
    |> Seq.sortByDescending (fun x -> x.Value)
    |> Seq.iter (fun x -> printfn "%s : %d" x.Key x.Value)





[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    evalutateAIs()
    
    let board = Board.create ()
    let player1 = createPlaryer Board.White
    let player2 = createPlaryer Board.Black
    let print = 
        System.Console.WriteLine()
        Console.print 
    gameLoop board player1 player2 print |> ignore
    0 // return an integer exit code
