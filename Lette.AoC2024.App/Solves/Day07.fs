namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day07 =

    type TestValue = TestValue of int64
    type Operand = Operand of int64
    type Equation = Equation of TestValue * Operand list
    type Operator = Operator of (int64 -> int64 -> int64)

    let parse input () =

        let testValueP = pint64 |>> TestValue
        let operandsP = sepBy' pint64 spaceP |>> List.map Operand
        let equationP = testValueP .>> pstring ": " .>>. operandsP |>> Equation
        let equationsP = sepBy' equationP newlineP

        Parser.run equationsP input

    //let concat a b = a * pown 10L (numberOfDigitsL b) + b
    let concat = (*) >> (>>) (numberOfDigitsL >> pown 10L) >> flip (>>) (+) >> dup

    let isEquationValid operators (Equation (TestValue t, operands)) =
        match operands with
        | [] -> failwith "no operands!"
        | Operand o :: [] -> o = t
        | Operand firstOperand :: restOfOperands ->

            let matchesTestValue opCombo =
                let rec loop operations acc =
                    match operations with
                    | [] -> acc = t
                    | (Operator operator, Operand operand) :: operations' ->
                        let acc' = operator acc operand
                        if acc' > t then
                            false
                        else
                            loop operations' acc'
                loop (List.zip opCombo restOfOperands) firstOperand

            operators
            |> allCombinations (List.length restOfOperands)
            |> Seq.exists matchesTestValue

    let part1 input =

        let operators = [Operator (+); Operator (*)]

        input
        |> List.filter (isEquationValid operators)
        |> List.map (fun (Equation (TestValue t, _)) -> t)
        |> List.sum

    let part2 input =

        let operators = [Operator (+); Operator (*); Operator concat]

        input
        |> List.filter (isEquationValid operators)
        |> List.map (fun (Equation (TestValue t, _)) -> t)
        |> List.sum

    let puzzle =
        Puzzle.init
            7
            (getInput >> parse)
            part1 (Some 7710205485870L)
            part2 (Some 20928985450275L)
