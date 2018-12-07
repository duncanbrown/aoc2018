module DayFive

open System.IO

let reactivePairs =
    List.zip ['a'..'z'] ['A'..'Z']
    |> List.append (List.zip ['A'..'Z'] ['a'..'z'])
    |> Set.ofList

let reducePolymer polymer =
    let rec loop (p: char[]) =
        let pairs = Seq.pairwise p
        match Seq.tryFindIndex (fun p -> Set.contains p reactivePairs) pairs with
        | Some i ->
            let newP = Array.append p.[0..i-1] p.[i+2..]
            loop newP
        | None -> p
    polymer |> Array.ofSeq |> loop |> Seq.length


let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\5.txt"
    reducePolymer (input.Trim ())