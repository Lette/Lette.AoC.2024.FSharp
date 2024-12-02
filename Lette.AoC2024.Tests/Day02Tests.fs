namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day02Tests =

    open Day02

    let input = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [
            [7; 6; 4; 2; 1]
            [1; 2; 7; 8; 9]
            [9; 7; 6; 2; 1]
            [1; 3; 2; 4; 5]
            [8; 6; 4; 4; 1]
            [1; 3; 6; 7; 9]
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 2

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 4
