module DaySix

open System
open System.IO



let sizeOfLargestArea points =    
    let minX = points |> List.map fst |> List.min
    let minY = points |> List.map snd |> List.min
    let maxX = points |> List.map fst |> List.max
    let maxY = points |> List.map snd |> List.max
    let buffer = 1000
    let grid =
        List.allPairs
            [minX - buffer .. maxX + buffer]
            [minY - buffer .. maxY + buffer]
    let isAtEdge (x,y) =
        x = minX - buffer ||
        x = maxX + buffer ||
        y = minY - buffer ||
        y = maxY + buffer
    let distance (x: int,y: int) (x',y') =
        (Math.Max (x,x')) - (Math.Min (x,x')) +
        (Math.Max (y,y')) - (Math.Min (y,y'))
    let closestPoint p =
        let distances = 
            points
            |> List.mapi (fun i p' -> i, distance p p')
        let minDistance = distances |> List.minBy snd |> snd
        let closests = List.filter (fun (_,d) -> d = minDistance) distances
        if List.length closests > 1
        then None
        else Some (closests |> List.head |> fst)
    let populatedGrid = 
        grid
        |> List.map (fun p -> p , closestPoint p)
    let maxFiniteArea = 
        populatedGrid
        |> List.filter (fun (_, closest) -> closest.IsSome)
        |> List.map (fun (p, closest) -> p, closest.Value)
        |> List.groupBy (fun (_, c) -> c)
        |> List.map (fun (k,v) -> k, List.map fst v)
        |> List.filter (fun (_,v) -> v |> List.exists isAtEdge |> not)
        |> List.map (fun (_,v) -> List.length v)
        |> List.max
    maxFiniteArea

let part1 () =
    let input = File.ReadAllLines "c:\\dev\\aoc2018\\input\\6.txt"
    let points =
        input
        |> Array.map (fun s -> (s.Trim ()).Split (',') )
        |> Array.map (fun a -> int a.[0] , int a.[1])
        |> List.ofArray
    sizeOfLargestArea points