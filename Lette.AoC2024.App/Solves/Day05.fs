namespace Lette.AoC2024

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day05 =

    let parse input () =

        let orderP = pint32 .>> pchar '|' .>>. pint32
        let ordersP = sepBy' orderP newlineP

        let updateP = sepBy' pint32 (pchar ',')
        let updatesP = sepBy' updateP newlineP

        let allP = ordersP .>> newlineP .>> newlineP .>>. updatesP

        Parser.run allP input

    let allOrderedPairs xs =
        let rec loop xs acc =
            match xs with
            | [] -> acc
            | x :: xs' ->
                let rec loopInner ys acc =
                    match ys with
                    | [] -> acc
                    | y :: ys' ->
                        loopInner ys' ((x, y) :: acc)
                loop xs' (loopInner xs' acc)

        loop xs []

    let getMiddleElement list =
        List.item ((List.length list - 1) / 2) list

    let getMappings orderings =
        lazy (
            let orderMappings =
                orderings
                |> List.groupBy fst
                |> List.map (fun (key, pairs)  -> (key, (pairs |> List.map snd)))

            let isInOrderMappings x y =
                List.exists (fun (k, ys) -> k = x && List.contains y ys) orderMappings

            let isUpdateInCorrectOrder update =
                let rec loop pairs =
                    match pairs with
                    | [] -> true
                    | (x, y) :: xs ->
                        if isInOrderMappings y x then
                            false
                        else
                            loop xs

                loop (allOrderedPairs update)

            (orderMappings, isUpdateInCorrectOrder)
        )

    let part1 (orderings, updates) =

        let _, isUpdateInCorrectOrder = (getMappings orderings).Value

        updates
        |> List.filter isUpdateInCorrectOrder
        |> List.map getMiddleElement
        |> List.sum

    let part2 (orderings, updates) =

        let orderMappings, isUpdateInCorrectOrder = (getMappings orderings).Value

        let rearrange update =

            let orderMappingsForUpdate =
                orderMappings
                |> List.filter (fun (key, _) -> List.contains key update)
                |> List.map (fun (k, ps) -> (k, ps |> List.filter (fun p -> List.contains p update)))

            let findMappingsOrDefault x =
                orderMappingsForUpdate
                |> List.tryFind (fst >> (=) x)
                |> Option.map snd
                |> Option.defaultValue []

            update
            |> List.map (fun k -> (k, findMappingsOrDefault k))
            |> List.sortBy (snd >> List.length)
            |> List.map fst

        updates
        |> List.filter (isUpdateInCorrectOrder >> not)
        |> List.map rearrange
        |> List.map getMiddleElement
        |> List.sum

    let puzzle =
        Puzzle.init
            5
            (getInput >> parse)
            part1 (Some 3608)
            part2 (Some 4922)
