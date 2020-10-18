namespace Othello

open System
open Board

module Console =
    let colorToChar color =
        match color with
        | None -> ' '
        | Some Board.White -> 'X'
        | Some Board.Black -> 'O'

    let print (board: Board.State) =
        printfn "   |a|b|c|d|e|f|g|h|"

        for y in Board.rows do
            (printf "%d: " y

             for x in Board.cols do
                 printf "|%c" (colorToChar (Board.get (x, y) board))

             printfn "|")

    let private parseIndex (validCharacters: string) (char: string) =
        let index =
            validCharacters.IndexOf(char, StringComparison.OrdinalIgnoreCase)

        if index <> -1
        then Ok index
        else Error "Invalid column enter a single character a-h"

    let private readRow () =
        printf "Row (0-7): "
        let line = Console.ReadLine().Trim()

        match Int32.TryParse line with
        | (true, value) when value >= 0 && value <= 7 -> Ok value
        | (true, _) -> Error "Value must be between 0 and 7 inclusive"
        | (false, _) -> Error "Row must be a number"

    let private readColumn () =
        printf "Column (a-h): "
        let line = Console.ReadLine().Trim()
        parseIndex "abcdefgh" line

    let rec getUserInput color board =
        printfn "\nPlease enter location for %As next move: " (colorToChar (Some color))

        let parseResult =
            readColumn ()
            |> Result.bind (fun x -> readRow () |> Result.map (fun y -> (x, y)))
            |> Result.bind (fun pos ->
                if (Board.isMoveValid pos color board)
                then Ok pos
                else Error(sprintf "Cannot place marker at %A" pos))

        match parseResult with
        | Ok pos -> pos
        | Error error ->
            (printfn "\nInvalid input: [%s]\n" error
             print board
             printfn ""
             getUserInput color board)

    let showGameResults board =
        printfn " -- END OF GAME -- "
        print board
        printfn ""

        let allpos = Seq.allPairs Board.rows Board.cols

        let (w, b) =
            Seq.fold (fun (numWhite, numBlack) (x, y) ->
                match Board.get (x, y) board with
                | Some Board.White -> (numWhite + 1, numBlack)
                | Some Board.Black -> (numWhite, numBlack + 1)
                | None -> (numWhite, numBlack)) (0, 0) allpos

        printfn "SCORE:\n 'X': %d  <-> 'O': %d" w b

        let (winner, score, otherscore) =
            if (w > b) then (Board.White, (64-b), b)
            else (Board.Black, (64-w), w)
            
        printfn "WINNER is %A with score %d against %d" (colorToChar (Some winner)) score otherscore
