namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day02 =

    let parse input () =
        let reportP = sepBy' pint32 spaceP
        let reportsP = sepBy' reportP newlineP

        Parser.run reportsP input

    let rec isSafe =
        function
        | a :: b :: _      when a = b || abs (a - b) > 3     -> false
        | a :: b :: c :: _ when sign (a - b) <> sign (b - c) -> false
        | _ :: xs -> isSafe xs
        | _       -> true

    let subReports report =
        let rec loop ix acc =
            if ix = List.length report then
                acc
            else
                let sub = report[..(ix - 1)] @ report[(ix + 1)..]
                loop (ix + 1) (sub :: acc)

        loop 0 []

    let (|||) f g x = f x || g x

    let isSafeOrTolerated =
        isSafe
        |||
        (subReports >> List.exists isSafe)

    let part1 =
        List.filter isSafe >> List.length

    let part2 =
        List.filter isSafeOrTolerated >> List.length

    let puzzle =
        Puzzle.init
            2
            (getInput >> parse)
            part1 (Some 230)
            part2 (Some 301)
