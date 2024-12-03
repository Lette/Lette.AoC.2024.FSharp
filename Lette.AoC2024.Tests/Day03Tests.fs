namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day03Tests =

    open Day03

    let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [Mul (2, 4); Dont; Mul (5, 5); Mul (11, 8); Do; Mul (8, 5)]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 161

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 48
