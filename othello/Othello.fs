// Learn more about F# at http://fsharp.org

namespace Othello

open System

module Board =
    type Color =
        | White
        | Black

    type State = array<Option<Color>>

    let flipColor color =
        match color with
        | White -> Black
        | Black -> White

    let private size = 8
    let rows = [ 0 .. 7 ]
    let cols = rows
    let allPositions = List.allPairs rows cols

    let private index x y = y * size + x

    let private allDirections =
        [ 1, 1
          1, 0
          1, -1
          0, -1
          -1, -1
          -1, 0
          -1, 1
          0, 1 ]

    [<Struct>]
    type Board(array: State) =
        member internal __.Set (x, y) color = array.[index x y] = color

    /// Create a board in inital state
    let create () =
        [| for row in 0 .. 7 do
            for col in 0 .. 7 do
                yield
                    match row, col with
                    | 3, 3 -> Some White
                    | 3, 4 -> Some Black
                    | 4, 3 -> Some Black
                    | 4, 4 -> Some White
                    | _ -> None |]

    let get (x, y) (board: State) = board.[index x y]

    let private set (x, y) color (board: State) =
        board.[index x y] <- Some color
        board

    let copy (board: State) = Array.copy board

    
    let foldPositions f state =
        allPositions |> List.fold f state

    let foldCells f state (board: State) =
        foldPositions (fun state pos -> f state (get pos board)) state


    let foldi f state (board: State) =
        let folder =
            (fun state pos -> f state pos (get pos board))
        foldPositions folder state

    let sumBy f (board: State) = foldCells (fun acc x -> acc + f (x)) 0 board

    let private countFlips (x, y) (dx, dy) (color: Color) board =
        let rec countFlipsImpl (x, y) count =
            if x < 0 || x >= size || y < 0 || y >= size then
                0
            else
                match (get (x, y) board) with
                | None -> 0
                | Some c when c <> color -> countFlipsImpl (x + dx, y + dy) (count + 1)
                | _ -> count

        countFlipsImpl (x, y) 0


    let isMoveValid (x, y) color board =
        if get (x, y) board <> None then
            false
        else
            allDirections
            |> List.exists (fun (dx, dy) -> countFlips (x + dx, y + dy) (dx, dy) color board > 0)

    let getValidMoves color board =
        allPositions 
        |> Seq.filter (fun pos -> isMoveValid pos color board)

    /// Place marker of a specific color at (x,y) and get resulting board
    let move (x: int, y: int) color board =
        let rec doFlip (x, y) (dx, dy) color count board =
            if count > 0
            then (doFlip (x + dx, y + dy) (dx, dy) color (count - 1) (set (x, y) color board))

        let newboard = copy board

        allDirections
        |> List.iter (fun (dx, dy) ->
            let nextPos = (x + dx, y + dy)
            let flips = countFlips nextPos (dx, dy) color board
            doFlip nextPos (dx, dy) color flips newboard
            //   flips
            )

        set (x, y) color newboard

module Scoring =
    let private lateScoreBoard =
        [| 50; -10; 5; 3; 3; 5; -10;  50;
          -10;  -8; 3; 1; 1; 3;  -8; -10;
            5;   3; 2; 1; 1; 2;   3;   5;
            3;   1; 1; 1; 1; 1;   1;   3;
            3;   1; 1; 1; 1; 1;   1;   3;
            5;   3; 2; 1; 1; 2;   3;   5;
          -10;  -8; 3; 1; 1; 3;  -8; -10;
           50; -10; 5; 3; 3; 5; -10;  50 |]

    let private earlyScoreBoard =
        [| 50; -10;  5;  1;  1;  5; -10;  50;
          -10;  -8;  3; -1; -1;  3;  -8; -10;
            5;   3;  2; -1; -1;  2;   3;   5;
            1;  -1; -1; -1; -1; -1;  -1;   1;
            1;  -1; -1; -1; -1; -1;  -1;   1;
            5;   3;  2; -1; -1;  2;   3;   5;
          -10;  -8;  3; -1; -1;  3;  -8; -10;
           50; -10;  5;  1;  1;  5; -10;  50 |]

    /// Returns the differnce in number of moves the players can do
    let moveDifference board color = 
        let otherColor = Board.flipColor color
        Board.foldPositions (fun acc pos -> 
            if Board.isMoveValid pos color board then acc + 1
            else if Board.isMoveValid pos otherColor board then acc - 1
            else acc) 0

    let scoreDifference board color =
        Board.sumBy (fun cell ->
            match cell with
            | Some c when c = color -> 1
            | Some _ -> -1
            | None -> 0) board


    let scoreByArray board color (scoreBoard: int array) =
        Board.foldi (fun acc (x, y) cell ->
            match cell with
            | Some c when c = color -> acc + scoreBoard.[x + 8 * y]
            | Some _ -> acc - scoreBoard.[x + 8 * y]
            | None -> acc) 0 board

    let scorePositional board color =
        scoreByArray board color lateScoreBoard

    let scorePositional2 board color =
        let numPieces = Board.sumBy (fun cell ->
                match cell with
                | Some _ -> 1
                | None -> 0) board

        let scoreBoard = 
            if numPieces < 16 then earlyScoreBoard 
            else lateScoreBoard

        scoreByArray board color scoreBoard

    // Combine scoring functions f1 and f2 using weights 
    let combine w1 f1 w2 f2 =
        (fun (board:Board.State) (color:Board.Color) ->
            (w1 * (f1 board color)) + (w2 * (f2 board color)))

module AI =
    let random () =
        let random = Random()

        (fun color board ->
            let moves =
                Board.getValidMoves color board |> Seq.toArray
            moves.[random.Next(moves.Length)])

    /// Pick move giving player the largest scoring giving a scoring method
    let greedy score =
        (fun color board ->
            Board.getValidMoves color board
            |> Seq.maxBy (fun x -> score (Board.move x color board) color))
