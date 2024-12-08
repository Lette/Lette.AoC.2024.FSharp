namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module Day08Tests =

    open Day08

    let input = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

    [<Fact>]
    let ``Parser works`` () =
        let input = "..0.
.A0.
A..."

        let result = parse input ()

        result |> should equal (
            array2D [
                [Empty;       Empty;       Antenna '0'; Empty]
                [Empty;       Antenna 'A'; Antenna '0'; Empty]
                [Antenna 'A'; Empty;       Empty;       Empty]
            ]
        )

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 14

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 34
