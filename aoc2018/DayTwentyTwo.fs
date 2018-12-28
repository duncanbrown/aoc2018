module DayTwentyTwo

type RegionType = Rocky | Wet | Narrow

let getRiskLevel depth target =
    let getErosionLevel geoIndex = (geoIndex + depth) % 20183
    let getType erosionLevel =
        match erosionLevel % 3 with
        | 0 -> Rocky
        | 1 -> Wet
        | 2 -> Narrow
    let getRiskLevel =
        function
        | Rocky  -> 0
        | Wet    -> 1
        | Narrow -> 2
    let targetX, targetY = target
    let points =
        List.concat
            [
                [for x in 0..targetX -> x,0];
                [for y in 1..targetY -> 0,y];
                [for y in 1..targetY do
                 for x in 1..targetX -> x,y]
            ]
    points
    |> List.fold
        (fun acc next ->
            let geoIndex =
                match next with
                | 0,0     -> 0
                | pos when pos = target  -> 0
                | x,0     -> x * 16807
                | 0,y     -> y * 48271
                | x,y     -> (Map.find (x-1,y) acc) * (Map.find (x,y-1) acc)
            let erosionLevel = getErosionLevel geoIndex
            Map.add next erosionLevel acc)
        Map.empty
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map getType
    |> Seq.sumBy getRiskLevel

let part1 () =
    getRiskLevel 11394 (7,701)