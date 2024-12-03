namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day03 =

    type Op =
        | Mul of int * int
        | Do
        | Dont

    let parse input () =

        let mulP = pstring "mul(" >>. pint32 .>> pchar ',' .>>. pint32 .>> pchar ')' |>> Mul
        let doP = pstring "do()" >>% Do
        let dontP = pstring "don't()" >>% Dont

        let opP = attempt (mulP <|> doP <|> dontP) |>> Some
        let skipP = skipAnyString 1 >>% None

        let rowP = many (opP <|> skipP)

        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input
        |> List.concat
        |> List.choose id

    let part1 input =

        let rec loop ops acc =
            match ops with
            | []               -> acc
            | Mul (a, b) :: xs -> loop xs (acc + a * b)
            | _          :: xs -> loop xs acc

        loop input 0

    let part2 input =

        let rec loop ops enabled acc =
            match ops, enabled with
            | []              , _    -> acc
            | Mul (a, b) :: xs, true -> loop xs true (acc + a * b)
            | Do         :: xs, _    -> loop xs true acc
            | Dont       :: xs, _    -> loop xs false acc
            | _          :: xs, _    -> loop xs enabled acc

        loop input true 0

    let puzzle =
        Puzzle.init
            3
            (getInput >> parse)
            part1 (Some 175615763)
            part2 (Some 74361272)
