module DayThree

open FParsec.Primitives
open FParsec
open System.IO
open System

type Claim = { Id: string; X: int; Y: int; Width: int; Height: int }

type Fabric = Map<(int * int), string list>

type InputParser = FParsec.Primitives.Parser<Claim list, unit>

//"#1 @ 1,3: 4x4"
let inputParser : InputParser = 
    let parseClaim =
        pchar '#'
        >>. pint32
        .>> pstring " @ "
        .>>. pint32
        .>> pchar ','
        .>>. pint32
        .>> pstring ": "
        .>>. pint32
        .>> pchar 'x'
        .>>. pint32
        .>> spaces
        |>> fun ((((id, x), y), width), height) -> {
            Id = string id;
            X = x;
            Y = y;
            Width = width;
            Height = height
        }
    many parseClaim

let emptyFabric: Fabric =
    List.allPairs [0..1000] [0..1000]
    |> List.map (fun p -> p, List.empty)
    |> Map.ofList

let makeClaim fabric claim =
    let points =
        List.allPairs
            [claim.X .. claim.X+claim.Width-1]
            [claim.Y .. claim.Y+claim.Height-1]
    let addClaimIdToPoint f p =
        let current =
            if Map.containsKey p f
            then Map.find p f
            else []
        let withClaimId = claim.Id :: current
        Map.add p withClaimId f        
    List.fold addClaimIdToPoint fabric points

let getOverlapCount claims =
    let fabric = List.fold makeClaim emptyFabric claims
    fabric
    |> Map.filter (fun _ v -> List.length v > 1)
    |> Map.count

let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\3.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (claims, _, _) -> getOverlapCount claims
    | Failure (err,_, _) -> raise(new Exception(err))