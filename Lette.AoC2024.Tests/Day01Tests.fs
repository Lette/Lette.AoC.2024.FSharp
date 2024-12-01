namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day01Tests =

    open Day01

    let input = "3   4
4   3
2   5
1   3
3   9
3   3"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal ([3; 4; 2; 1; 3; 3], [4; 3; 5; 3; 9; 3])

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 11

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 31
