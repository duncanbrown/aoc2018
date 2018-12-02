module DayTwo

open System.IO

let checksum input =
    let letterCounts (s: string) =
        Seq.groupBy id s
        |> Seq.map (fun (_,v) -> Seq.length v)
        |> Set.ofSeq
    let has n =
        input
        |> List.map letterCounts
        |> List.sumBy (fun counts -> if Set.contains n counts then 1 else 0)
    has 2 * has 3

let part1 () =
    let input = File.ReadAllLines "c:\\dev\\aoc2018\\input\\2.txt"
    input
    |> List.ofArray
    |> checksum