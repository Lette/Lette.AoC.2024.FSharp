namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day07Tests =

    open Day07

    let input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

    [<Fact>]
    let ``Parser works`` () =
        let input = "190: 10 19
3267: 81 40 27
83: 17 5"

        let result = parse input ()

        result |> should equal [
            Equation (TestValue 190, [Operand 10; Operand 19])
            Equation (TestValue 3267, [Operand 81; Operand 40; Operand 27])
            Equation (TestValue 83, [Operand 17; Operand 5])
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 3749L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 11387L
