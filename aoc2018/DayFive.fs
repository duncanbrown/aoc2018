module DayFive

open System.IO
open System
open Microsoft.FSharp
open Microsoft.FSharp.Collections
open FSharp.Collections.ParallelSeq
open System

let reactivePairs =
    List.zip ['a'..'z'] ['A'..'Z']
    |> List.append (List.zip ['A'..'Z'] ['a'..'z'])
    |> Set.ofList

let reducePolymer polymer =
    let rec loop (p: char[]) =
        let pairs = Seq.pairwise p
        match Seq.tryFindIndex (fun (c, c') -> c <> c' && (c = Char.ToLower c' || c = Char.ToUpper c')) pairs with
        | Some i ->
            let span = p.AsSpan()
            let newP = Array.append ((span.Slice (0, i)).ToArray ()) ((span.Slice (i+2)).ToArray ())
            loop newP
        | None -> p
    polymer |> Array.ofSeq |> loop |> Seq.length

let pruneAndReduce (polymer: string) =
    let units = ['a'..'z']
    let pruneUnitAndReduce unit =
        let pruned = polymer.Replace(string unit, "", StringComparison.InvariantCultureIgnoreCase)
        reducePolymer pruned    

    units
        |> PSeq.map pruneUnitAndReduce
        |> PSeq.min


let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\5.txt"
    reducePolymer (input.Trim ())
    
let part2 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\5.txt"
    pruneAndReduce (input.Trim ())