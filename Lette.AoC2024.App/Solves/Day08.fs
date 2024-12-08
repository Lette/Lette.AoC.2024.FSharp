namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day08 =

    type Cell =
        | Empty
        | Antenna of frequency: char

    let parse input () =

        let emptyP = pchar '.' >>% Empty
        let antennaP = (satisfy isDigit <|> satisfy isAsciiLetter) |>> Antenna
        let cellP = emptyP <|> antennaP
        let rowP = many cellP
        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input
        |> array2D

    let (++) (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)
    let (--) (r1, c1) (r2, c2) = (r1 - r2, c1 - c2)

    let init input =
        let height = Array2D.length1 input
        let width = Array2D.length2 input

        let antennas =
            [
                for r in 0 .. height - 1 do
                    for c in 0 .. width - 1 do
                        match input[r, c] with
                        | Empty             -> ()
                        | Antenna frequency -> yield (frequency, (r, c))
            ]
            |> List.groupBy fst
            |> List.map (fun (frequency, cells) -> (frequency, cells |> List.map snd))

        let isWithinBounds (r, c) =
            r >= 0 && r < height && c >= 0 && c < width

        (antennas, isWithinBounds)

    let processAntennas antennas antennaPairProcessor =

        let processAntenna cells acc =
            List.pairs cells
            |> List.map antennaPairProcessor
            |> cons acc
            |> List.concat

        let rec loop antennas acc =
            match antennas with
            | [] -> acc
            | (_, cells) :: antennas' -> loop antennas' (processAntenna cells acc)

        loop antennas []
        |> List.distinct
        |> List.length

    let part1 input =

        let antennas, isWithinBounds = init input

        let processAntennaPair (antenna1, antenna2) =
            let diff = antenna2 -- antenna1
            let antinode1 = antenna1 -- diff
            let antinode2 = antenna2 ++ diff

            [
                if isWithinBounds antinode1 then yield antinode1
                if isWithinBounds antinode2 then yield antinode2
            ]

        processAntennas antennas processAntennaPair

    let part2 input =

        let antennas, isWithinBounds = init input

        let processAntennaPair (antenna1, antenna2) =
            let rec getAntinodeUntilOutOfBounds position direction =
                [
                    if isWithinBounds position then
                        yield position
                        yield! getAntinodeUntilOutOfBounds (position ++ direction) direction
                ]

            [
                yield! getAntinodeUntilOutOfBounds antenna1 (antenna1 -- antenna2)
                yield! getAntinodeUntilOutOfBounds antenna2 (antenna2 -- antenna1)
            ]

        processAntennas antennas processAntennaPair

    let puzzle =
        Puzzle.init
            8
            (getInput >> parse)
            part1 (Some 222)
            part2 (Some 884)
