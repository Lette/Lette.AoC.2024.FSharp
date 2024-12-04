namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day04Tests =

    open Day04

    let input = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

    [<Fact>]
    let ``Parser works`` () =
        let smallerInput = "MMMS
MSAM
AMXS
MSAM"

        let result = parse smallerInput ()

        let expected =
            array2D [
                ['M'; 'M'; 'M'; 'S']
                ['M'; 'S'; 'A'; 'M']
                ['A'; 'M'; 'X'; 'S']
                ['M'; 'S'; 'A'; 'M']
            ]

        result |> should equal expected

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 18

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 9
