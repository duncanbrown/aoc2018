module DayEleven

open FSharp.Collections.ParallelSeq
open System.Linq
open System.Collections.Generic
open System.Collections.Generic
open System.Diagnostics


let getPowerLevel serialNumber (x,y)  =
    let rackId = x+10
    let power = rackId * y + serialNumber
    let power' = power * rackId
    let hundreds = (power' / 100) |> string |> Seq.last |> string |> int
    hundreds - 5

let getLargestTotalPower serialNumber =
    let powerLevels =
        List.allPairs [1..300] [1..300]
        |> List.map (fun p -> p, getPowerLevel serialNumber p)
        |> Map.ofList
    let squares =
        List.allPairs [1..298] [1..298]
        |> List.map (fun (x,y) -> List.allPairs [x..x+2] [y..y+2])
    squares
        |> List.maxBy (List.sumBy (fun p -> Map.find p powerLevels))
        |> List.head

let getLargestTotalPowerOfAnySize serialNumber =    

    let powerLevelsBySquareSize: Dictionary<int, Dictionary<int * int, int>> = Dictionary()
    [1..300] |> List.iter (fun i -> powerLevelsBySquareSize.[i] <- Dictionary())

    let powerLevels =
        [|
            for y in [1..300] ->
                [|
                    for x in [1..300] ->
                        let powerLevel = getPowerLevel serialNumber (x,y)
                        powerLevel
                |]
        |]
    

    let startingPointsBySquareSize =
        [1..300] |> List.map (fun size -> size, List.allPairs [1..(301 - size)] [1..(301 - size)])

    let getPowerLevelForSquareOfSizeStartingAt size startPoint =
        let previous = if size = 1 then 0 else powerLevelsBySquareSize.[size-1].[startPoint]
        let startX, startY = startPoint
        let newPoints =
            if size = 1
            then [(startX, startY)]
            else
                List.append
                    [for x in startX .. (startX+size-1) -> (x,startY+size-1)]
                    [for y in startY .. (startY+size-2) -> (startX+size-1,y)]
        let newPointsSum =
            newPoints
            |> List.sumBy (fun (x,y) -> powerLevels.[y-1].[x-1])
        let squareSum = previous + newPointsSum
        powerLevelsBySquareSize.[size].[startPoint] <- squareSum
        squareSum

    let (s, p, _) =
        startingPointsBySquareSize
        |> Seq.map (fun (size, points) ->
            printfn "%i" size
            let powerLevels = points |> List.map (fun p -> p, getPowerLevelForSquareOfSizeStartingAt size p)
            let maxStartingPoint, maxPowerLevel = powerLevels |> List.maxBy snd
            size, maxStartingPoint, maxPowerLevel)
        |> Seq.maxBy (fun (_, _, power) -> power)
    s, p

let part1 () =
    getLargestTotalPower 5235

let part2 () =
    getLargestTotalPowerOfAnySize 5235