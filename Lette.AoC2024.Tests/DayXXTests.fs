namespace Lette.AoC2024

open Xunit
open FsUnit.Xunit

module DayXXTests =

    open DayXX

    let input = " "

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [Some ' ']

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 0

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 0
