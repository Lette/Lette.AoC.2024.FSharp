namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day01 =

    let parse input () =

        let rowP = pint32 .>> spaces .>>. pint32

        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input
        |> List.unzip

    let distance a b = abs (a - b)

    let part1 input =

        input
        |> Tuple.map1 List.sort
        ||> List.map2 distance
        |> List.sum

    let appearancesMap =
        List.countBy id >> Map.ofList

    let getAppearances =
        flip (Map.findOrDefault 0)

    let part2 (left, right) =

        let getAppearances' =
            getAppearances (appearancesMap right)

        let similarityScore x =
            x * (getAppearances' x)

        left
        |> List.map similarityScore
        |> List.sum

    let puzzle =
        Puzzle.init
            1
            (getInput >> parse)
            part1 (Some 1590491)
            part2 (Some 22588371)
