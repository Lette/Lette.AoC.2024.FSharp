namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day06Tests =

    open Day06

    let input = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

    [<Fact>]
    let ``Parser works`` () =
        let smallerInput = ".#..
..#.
#^..
....
.#.."
        let result = parse smallerInput ()

        result |> should equal (
            [
                [Empty; Blocked; Empty; Empty]
                [Empty; Empty; Blocked; Empty]
                [Blocked; Guard; Empty; Empty]
                [Empty; Empty; Empty; Empty]
                [Empty; Blocked; Empty; Empty]
            ] |> array2D
        )

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 41

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 6
