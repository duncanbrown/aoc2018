module DayTwentyTwo

open FSharp.Collections.ParallelSeq
open System.Diagnostics

type RegionType = Rocky | Wet | Narrow

let buildWorld depth target =
    let getErosionLevel geoIndex = (geoIndex + depth) % 20183
    let getType erosionLevel =
        match erosionLevel % 3 with
        | 0 -> Rocky
        | 1 -> Wet
        | 2 -> Narrow
    let targetX, targetY = target
    let maxX = targetX + 100
    let maxY = targetY + 100
    let points =
        List.concat
            [
                [for x in 0..maxX -> x,0];
                [for y in 1..maxY -> 0,y];
                [for y in 1..maxY do
                 for x in 1..maxX -> x,y]
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
    |> Map.map (fun _ v -> getType v)

let getRiskLevel depth target =
    let getRiskLevel =
        function
        | Rocky  -> 0
        | Wet    -> 1
        | Narrow -> 2
    buildWorld depth target    
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sumBy getRiskLevel

type Equipment = Torch | ClimbingGear | Neither
type EquipmentState =
    Wearing of Equipment
    | SwitchingTo of Equipment * int
    
let validEquipmentFor =
    function
    | Rocky  -> [ClimbingGear;Torch]
    | Wet    -> [ClimbingGear;Neither]
    | Narrow -> [Torch;Neither]

let findShortestRoute world target =
    let getRegionType p = Map.find p world
    let getOptions equipmentState position equipmentAtPositions =
        match equipmentState with
        | SwitchingTo (equipment,wait) ->
            if wait > 1
            then Choice1Of2 [|SwitchingTo(equipment,wait-1), position|]
            else Choice1Of2 [|Wearing equipment, position|]
        | Wearing(equipment) ->
            if position = target && equipment = Torch
            then Choice2Of2 ()
            else if position = target
            then Choice1Of2 [|SwitchingTo(Torch, 6), position|]
            else
                let x,y = position
                let targetX, targetY = target
                let moveCandidates =
                    [x,y-1;x,y+1;x-1,y;x+1,y]
                    |> List.filter (fun (x',y') ->
                        x' >= 0 && x' <= targetX + 100  &&
                        y' >= 0 && y' <= targetY + 100 )
                    |> List.map (fun p ->
                        p, p |> getRegionType |> validEquipmentFor, Map.tryFind p equipmentAtPositions)
                let moveNowOptions =
                    moveCandidates
                    |> List.filter (fun (_,validEquipment,previousAtPositionO) ->
                        List.contains equipment validEquipment &&
                        match previousAtPositionO with
                        | Some es ->
                            match Map.tryFind equipment es with
                            | Some n -> n > 1
                            | _ -> true
                        | _ -> true)
                let newEquipment =
                    position
                    |> getRegionType
                    |> validEquipmentFor
                    |> List.except [equipment]
                    |> List.head
                let moveAfterSwitchOptions =
                    moveCandidates
                    |> List.filter (fun (_,validEquipment,previousAtPositionO) ->
                        List.contains newEquipment validEquipment &&
                        match previousAtPositionO with
                        | Some es ->
                            match Map.tryFind equipment es with
                            | Some _ -> false
                            | _ -> true
                        | _ -> true)
                let shouldSwitch =
                    (not (List.isEmpty moveAfterSwitchOptions)) &&
                    (match Map.tryFind position equipmentAtPositions with
                     | Some es -> not (Map.containsKey newEquipment es)
                     | _ -> true)
                let options =
                    List.append
                        (moveNowOptions |> List.map (fun (p,_,_) -> equipmentState, p))
                        (if shouldSwitch then [SwitchingTo(newEquipment, 6), position] else [])
                Choice1Of2 (options |> Array.ofList)
    let rec loop paths equipmentAtPositions i =
        let sw = Stopwatch.StartNew()
        printfn "%i %i" i (Array.length paths)

        let choices =
            paths
            |> PSeq.distinct
            |> PSeq.map (fun (state, pos) -> getOptions state pos equipmentAtPositions)
            |> PSeq.toArray
        if choices |> Array.exists (function Choice2Of2 _ -> true | _ -> false)
        then i
        else
            let paths' =
                choices
                |> Array.map
                    (fun c ->
                        let (Choice1Of2 p) = c
                        p)
                |> Array.concat
            let equipmentAtPositions' =
                paths'
                |> Array.filter
                    (fun p ->
                        match fst p with
                        | Wearing _ -> true
                        | _         -> false)
                |> Array.fold
                    (fun acc next ->
                        let es, pos = next
                        match es with
                        | SwitchingTo (e,n) ->
                            match Map.tryFind pos acc with
                            | Some es ->
                                match Map.tryFind e es with
                                | Some n' -> if n < n' then Map.add pos (Map.add e n es) acc else acc
                                | _ -> Map.add pos (Map.add e n es) acc
                            | None    -> Map.add pos (Map.ofList [e,n]) acc
                        | Wearing e ->
                            match Map.tryFind pos acc with
                            | Some es ->
                                match Map.tryFind e es with
                                | Some _ -> Map.add pos (Map.add e 0 es) acc
                                | _ -> Map.add pos (Map.add e 0 es) acc
                            | None    -> Map.add pos (Map.ofList [e,0]) acc)
                    equipmentAtPositions
            printfn "%A" sw.Elapsed
            loop paths' equipmentAtPositions' (i+1)
    loop [|Wearing Torch, (0,0)|] (Map.ofList [(0,0), (Map.ofList [Torch,0])]) 0



let part1 () =
    getRiskLevel 510 (10,10)

let part2 () =
    //let world = buildWorld 510 (10,10)
    //findShortestRoute world (10,10)
    let world = buildWorld 11394 (7,701)
    findShortestRoute world (7,701)