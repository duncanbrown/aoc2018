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

let findCommon (input: string list) =
    let length = input.[0].Length
    input
        |> Seq.map (fun s ->
            input
                |> Seq.filter (fun s' -> s <> s')
                |> Seq.map (fun s' -> s, s'))
        |> Seq.concat
        |> Seq.map (fun (s, s') ->
            Seq.zip s s'
                |> Seq.filter (fun (c, c') -> c = c' )
                |> Seq.map fst)
        |> Seq.find (fun s -> (Seq.length s) = length - 1)
        |> Array.ofSeq
        |> System.String.Concat  


let part1 () =
    let input = File.ReadAllLines "c:\\dev\\aoc2018\\input\\2.txt"
    input
    |> List.ofArray
    |> checksum

let part2 () =
    let input = File.ReadAllLines "c:\\dev\\aoc2018\\input\\2.txt"
    input
    |> List.ofArray
    |> findCommon

