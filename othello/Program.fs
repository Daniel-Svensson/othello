// Learn more about F# at http://fsharp.org

namespace Othello

open System

module Board =
    type Color = White | Black
    type State = array<Option<Color>>

    let flipColor color = 
        match color with 
        | White -> Black
        | Black -> White

    let private size = 8
    let rows = [0..7]
    let cols = rows
    let private index x y = y * size + x
    let private allDirections = [1,1; 1,0; 1,-1; 0,-1; -1,-1; -1,0; -1,1; 0,1]

    [<Struct>]
    type Board(array:State) =

        member internal __.set (x,y) color =
            array.[index x y] = color

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

    let get (x, y) (board :State) = 
        board.[index x y]
    
    let private set (x, y) color (board :State) = 
        board.[index x y] <- Some color
        board

    let copy (board:State) = Array.copy board

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

module Scoring =
    let scoreDifference board color =
        Seq.allPairs Board.rows Board.cols
        |> Seq.sumBy (fun (x,y) -> 
            match Board.get (x,y) board with
            | Some c when c = color -> 1
            | Some _-> -1
            | None -> 0)

module AI =
    let random() =
        let random = Random()
        (fun color board -> 
            let moves = Board.getValidMoves color board |> Seq.toArray
            moves.[random.Next(0, moves.Length-1)])

    /// Pick move giving player the largest scoring giving a scoring method
    let greedy score =
        (fun color board -> 
            Board.getValidMoves color board
            |> Seq.maxBy (fun x -> score board color))

