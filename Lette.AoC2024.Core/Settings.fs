namespace Lette.AoC2024

type DaySetting =
    | All
    | Latest
    | Only of int list
    | Except of int list

type PartSetting =
    | Both
    | First
    | Second

type Settings =
    {
        Days: DaySetting
        Parts: PartSetting
        ShowParserTime: bool
    }

[<RequireQualifiedAccess>]
module Settings =

    let withDay day settings =
        let days =
            match settings.Days with
            | Only ds -> Only (day :: ds)
            | _       -> Only (day :: [])
        { settings with Days = days }

    let withoutDay day settings =
        let days =
            match settings.Days with
            | Except ds -> Except (day :: ds)
            | _         -> Except (day :: [])
        { settings with Days = days }

    let withLatest settings =
        { settings with Days = Latest }

    let withAll settings =
        { settings with Days = All; Parts = Both }

    let withPart part settings =
        { settings with Parts = part }

    let withParserTime settings =
        { settings with ShowParserTime = true }

    let defaults =
        {
            Days = All
            Parts = Both
            ShowParserTime = false
        }
