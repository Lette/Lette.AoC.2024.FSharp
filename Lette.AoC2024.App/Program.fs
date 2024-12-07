namespace Lette.AoC2024

module Program =

    let parseArgs args =
        let args' = args |> List.ofArray

        let rec loop args acc =
            match args with
            | [] -> acc

            | "-l"                        :: xs -> loop xs (Settings.withLatest acc)
            | "--latest"                  :: xs -> loop xs (Settings.withLatest acc)

            | "-a"                        :: xs -> loop xs (Settings.withAll acc)
            | "--all"                     :: xs -> loop xs (Settings.withAll acc)

            | "-d"                        :: [] -> failwith "-d must be followed by a valid day"
            | "-d"       :: IsInteger day :: xs -> loop xs (Settings.withDay day acc)
            | "-d"       :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"
            | "--day"                     :: [] -> failwith "--day must be followed by a valid day"
            | "--day"    :: IsInteger day :: xs -> loop xs (Settings.withDay day acc)
            | "--day"    :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"

            | "-e"                        :: [] -> failwith "-e must be followed by a valid day"
            | "-e"       :: IsInteger day :: xs -> loop xs (Settings.withoutDay day acc)
            | "-e"       :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"
            | "--except"                  :: [] -> failwith "--except must be followed by a valid day"
            | "--except" :: IsInteger day :: xs -> loop xs (Settings.withoutDay day acc)
            | "--except" :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"

            | "-p"                        :: [] -> failwith "-p must be followed by a valid part (1 or 2)"
            | "-p"       :: "1"           :: xs -> loop xs (Settings.withPart First acc)
            | "-p"       :: "2"           :: xs -> loop xs (Settings.withPart Second acc)
            | "-p"       :: bad           :: _  -> failwith $"'%s{bad}' is an unknown part"
            | "--part"                    :: [] -> failwith "--part must be followed by a valid part (1 or 2)"
            | "--part"   :: "1"           :: xs -> loop xs (Settings.withPart First acc)
            | "--part"   :: "2"           :: xs -> loop xs (Settings.withPart Second acc)
            | "--part"   :: bad           :: _  -> failwith $"'%s{bad}' is an unknown part"

            | "-t"                        :: xs -> loop xs (Settings.withParserTime acc)
            | "--show-parser-time"        :: xs -> loop xs (Settings.withParserTime acc)

            | s                           :: _  -> failwith $"'%s{s}' is an unknown argument"

        loop args' Settings.defaults

    [<EntryPoint>]
    let main args =

        let settings = parseArgs args

        Timing.displayTimerProperties ()

        let puzzles =
            [
                Day01.puzzle
                Day02.puzzle
                Day03.puzzle
                Day04.puzzle
                Day05.puzzle
                Day06.puzzle
            ] : IPuzzle list

        Presentation.printHeader ()

        Puzzle.run puzzles settings
        |> Seq.tap Presentation.printResult
        |> Seq.where Puzzle.isUnsuccessful
        |> Seq.length
