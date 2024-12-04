namespace Lette.AoC2024

open FParsec

module Day04 =

    let parse input () =

        let charP = pchar 'X' <|> pchar 'M' <|> pchar 'A' <|> pchar 'S'
        let rowP = many1 charP
        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input
        |> array2D

    let part1 =

        let patterns = [
            [(0, 0, 'X'); (0, 1, 'M'); (0, 2, 'A'); (0, 3, 'S')]
            [(0, 0, 'S'); (0, 1, 'A'); (0, 2, 'M'); (0, 3, 'X')]
            [(0, 0, 'X'); (1, 0, 'M'); (2, 0, 'A'); (3, 0, 'S')]
            [(0, 0, 'S'); (1, 0, 'A'); (2, 0, 'M'); (3, 0, 'X')]
            [(0, 0, 'X'); (1, 1, 'M'); (2, 2, 'A'); (3, 3, 'S')]
            [(0, 0, 'S'); (1, 1, 'A'); (2, 2, 'M'); (3, 3, 'X')]
            [(0, 3, 'X'); (1, 2, 'M'); (2, 1, 'A'); (3, 0, 'S')]
            [(0, 3, 'S'); (1, 2, 'A'); (2, 1, 'M'); (3, 0, 'X')]
        ]

        Pattern2D.scanMany patterns >> List.length

    let part2 =

        let patterns = [
            [(0, 0, 'M'); (1, 1, 'A'); (2, 2, 'S'); (0, 2, 'M'); (2, 0, 'S')]
            [(2, 2, 'M'); (1, 1, 'A'); (0, 0, 'S'); (0, 2, 'M'); (2, 0, 'S')]
            [(2, 2, 'M'); (1, 1, 'A'); (0, 0, 'S'); (2, 0, 'M'); (0, 2, 'S')]
            [(0, 0, 'M'); (1, 1, 'A'); (2, 2, 'S'); (2, 0, 'M'); (0, 2, 'S')]
        ]

        Pattern2D.scanMany patterns >> List.length

    let puzzle =
        Puzzle.init
            4
            (getInput >> parse)
            part1 (Some 2390)
            part2 (Some 1809)
