namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day06 =

    type Cell =
        | Empty
        | Blocked
        | Guard

    let parse input () =

        let emptyP = pchar '.' >>% Empty
        let blockedP = pchar '#' >>% Blocked
        let guardP = pchar '^' >>% Guard

        let cellP = emptyP <|> blockedP <|> guardP
        let rowP = many cellP
        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input
        |> array2D

    let height input = Array2D.length1 input
    let width input = Array2D.length2 input

    let isWithinBounds input =
        let h = height input
        let w = width input

        fun (r, c) -> r >= 0 && r < h && c >= 0 && c < w

    let getCell input (r, c) = Array2D.get input r c
    let setCell input (r, c) v = Array2D.set input r c v

    let tryGetCell input position =
        if isWithinBounds input position then
            Some (getCell input position)
        else
            None

    let moveForward (r, c) (dr, dc) = (r + dr, c + dc)
    let turnRight (dr, dc) = (dc, -dr)

    let part1 input =

        let set' = setCell input

        let guardPosition = input |> Array2D.find Guard
        let guardDirection = (-1, 0)

        set' guardPosition Empty

        let visited = Set.empty |> Set.add guardPosition

        let rec loop position direction acc =
            let nextPosition = moveForward position direction
            match tryGetCell input nextPosition with
            | None         -> acc
            | Some Empty   -> loop nextPosition direction             (acc |> Set.add nextPosition)
            | Some Blocked -> loop position     (turnRight direction) acc
            | _ -> failwith "Unexpected cell"

        loop guardPosition guardDirection visited
        |> Set.count

    let part2 input =

        let set' = setCell input

        let guardPosition = input |> Array2D.find Guard
        let guardDirection = (-1, 0)

        set' guardPosition Empty

        let visited = Set.empty |> Set.add (guardPosition, guardDirection)

        let isPossibleObstruction obstructionPosition position direction visited =

            if visited |> Set.exists (fun (p, _) -> p = obstructionPosition) then
                false
            else
                set' obstructionPosition Blocked

                let rec loop position direction visited =
                    let nextPosition = moveForward position direction
                    match tryGetCell input nextPosition with
                    | None         -> false
                    | Some Empty   ->
                        if visited |> Set.contains (nextPosition, direction) then
                            true
                        else
                            let visited' = visited |> Set.add (nextPosition, direction)
                            loop nextPosition direction visited'
                    | Some Blocked ->
                        let direction' = turnRight direction
                        if visited |> Set.contains (position, direction') then
                            true
                        else
                            let visited' = visited |> Set.add (position, direction')
                            loop position direction' visited'
                    | _ -> failwith "Unexpected cell"

                let isLoop = loop position direction visited

                set' obstructionPosition Empty
                isLoop

        let rec loop position direction visited obstructions =
            let nextPosition = moveForward position direction
            match tryGetCell input nextPosition with
            | None -> obstructions
            | Some Empty ->
                let obstructions' =
                    if isPossibleObstruction nextPosition position direction visited then
                        (nextPosition :: obstructions)
                    else
                        obstructions
                let visited' = visited |> Set.add (nextPosition, direction)
                loop nextPosition direction visited' obstructions'
            | Some Blocked ->
                let direction' = turnRight direction
                let visited' = visited |> Set.add (position, direction')
                loop position direction' visited' obstructions
            | _ -> failwith "Unexpected cell"

        loop guardPosition guardDirection visited []
        |> List.distinct
        |> List.length

    let puzzle =
        Puzzle.init
            6
            (getInput >> parse)
            part1 (Some 5145)
            part2 (Some 1523)
