// Learn more about F# at http://fsharp.org

open System

module Board =
    type Color = White | Black
    
    let flipColor color = 
        match color with 
        | White -> Black
        | Black -> White

    let private size = 8
    let rows = [0..7]
    let cols = rows
    let private index x y = y * size + x
    let private allDirections = [1,1; 
        1,0; 
        1,-1; 
        0,-1;
        -1,-1;
        -1,0;
        -1,1;
        0,1]

    /// Create a board in inital state
    let create () = 
        [|for row in 0..7 do
            for col in 0..7 do
                yield 
                    match row, col with
                    | 3,3 -> Some White
                    | 3,4 -> Some Black
                    | 4,3 -> Some Black
                    | 4,4 -> Some White
                    | _ -> None|]

    
    let get (x, y) (board :array<Option<Color>>) = 
        board.[index x y]
    
    let private set (x, y) color (board :array<Option<Color>>) = 
        board.[index x y] <- Some color
        board

    let copy board = Array.copy board

    let private countFlips (x, y) (dx, dy) (color:Color) board =
        let rec countFlipsImpl (x, y) count =
            if x < 0 || x >= size || y < 0 || y >= size then 0
            else 
                match (get (x, y) board) with
                | None -> 0
                | Some c when c <> color -> countFlipsImpl (x+dx, y+dy) (count+1)
                | _ -> count
        countFlipsImpl (x,y) 0


    let isValid (x, y) color board =
        if get (x, y) board <> None then 
            false
        else
            allDirections
            |> List.exists(fun (dx, dy) ->
                countFlips (x+dx, y+dy) (dx,dy) color board > 0)

    let getValidMoves color board = 
        seq {
            for y in rows do
                for x in cols do
                    if (isValid (x,y) color board) then
                        yield (x,y)
        }

    /// Place marker of a specific color at (x,y) and get resulting board
    let move (x:int, y:int) color board =
        let rec doFlip (x,y) (dx,dy) color count board =
            if count > 0 then begin
                doFlip (x+dx,y+dy) (dx,dy) color (count-1) (set (x,y) color board)
            end

        let newboard = copy board
        allDirections 
        |> List.iter (fun (dx, dy) ->
            let nextPos = (x+dx, y+dy)
            let flips = countFlips nextPos (dx,dy) color board 
            doFlip nextPos (dx,dy) color flips newboard
         //   flips
        )
        set (x,y) color newboard


module Console =

    let colorToChar color =
        match color with
        | None -> ' '
        | Some Board.White -> 'X'
        | Some Board.Black -> 'O'

    let print (board:array<option<Board.Color>>) =
        printfn "   |a|b|c|d|e|f|g|h|"
        for y in Board.rows do
        begin
            printf "%d: " y
            for x in Board.cols do
                printf "|%c" (colorToChar (Board.get (x,y) board))
            printfn "|"
        end

    let private parseIndex (validCharacters:string) (char:string) =
        let index = validCharacters.IndexOf(char, StringComparison.OrdinalIgnoreCase)
        if index <> -1 then Ok index
        else Error "Invalid column enter a single character a-h"

    let private readRow() =
        printf "Row (0-7): "
        let line = Console.ReadLine().Trim()
        match Int32.TryParse line with
        | (true, value) when value >= 0 && value <= 7 -> Ok value
        | (true, _) -> Error "Value must be between 0 and 7 inclusive"
        | (false, _) -> Error "Row must be a number"

    let private readColumn() =
        printf "Column (a-h): "
        let line = Console.ReadLine().Trim()
        parseIndex "abcdefgh" line

    let rec getUserInput color board =
        print board
        printfn "\nPlease enter location for %As next move: " (colorToChar (Some color))
        let parseResult =
            readRow()
            |> Result.bind (fun y -> readColumn() |> Result.map (fun x -> (x,y)))
            |> Result.bind (fun pos -> 
                if (Board.isValid pos color board) then Ok pos
                else Error (sprintf "Cannot place marker at %A" pos))

        match parseResult with
        | Ok pos -> pos
        | Error error -> 
            begin
                printfn "\nInvalid input: [%s]\n" error
                getUserInput color board
            end


let showGameResults board =
    printfn "GAME OVER"
    let allpos = Seq.allPairs Board.rows Board.cols
    let (w, b) = Seq.fold (fun (numWhite, numBlack) (x,y) -> 
                    match Board.get (x,y) board with
                    | Some Board.White -> (numWhite+1, numBlack)
                    | Some Board.Black -> (numWhite, numBlack+1)
                    | None -> (numWhite, numBlack)
                    ) (0,0) allpos
    printfn "SCORE:\n 'X': %d  <-> 'O': %d" w b
    let winner = if (w > b) then Board.White else Board.Black
    printfn "WINNER is %A" (Console.colorToChar (Some winner))

let rec gameLoop board color =
    if (Seq.isEmpty (Board.getValidMoves color board)) then
        showGameResults board
    else
        begin
            let move = Console.getUserInput color board
            let newBoard = Board.move move color board
            gameLoop newBoard (Board.flipColor color)
        end

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let board = Board.create()
    gameLoop board Board.White
    0 // return an integer exit code
