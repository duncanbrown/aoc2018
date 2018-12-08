module DaySeven

open FParsec
open System.IO
open System

type InputParser = FParsec.Primitives.Parser<(char * char) list, unit>
let inputParser: InputParser =
    let pInstruction =
        pstring "Step "
        >>. anyChar
        .>> pstring " must be finished before step "
        .>>. anyChar
        .>> pstring " can begin."
        .>> spaces
    many pInstruction

let addOrUpdate k f d m =
    match Map.tryFind k m with
    | Some v -> Map.add k (f v) m
    | None   -> Map.add k d m 

let contains c s =
    String.exists (fun c' -> c' = c) s

let order (instructions: (char * char) list) =
    let requisites =
        instructions
        |> List.fold
            (fun acc (requisite: char, forStep: char) ->                
                addOrUpdate
                    forStep
                    (Set.add requisite)
                    (Set.singleton requisite)
                    acc)
            Map.empty
    let noRequisites =
        instructions
        |> List.map fst
        |> List.filter (fun c -> not (Map.containsKey c requisites))
    let allRequisites =
        List.fold
            (fun acc c -> Map.add c Set.empty acc)
            requisites
            noRequisites
    let rec loop sequence =
        let candidate =
            allRequisites
            |> Map.filter (fun k _ -> not (contains k sequence))
            |> Map.filter (fun _ rs -> Set.forall (fun r -> contains r sequence) rs)
            |> Map.toList
            |> List.map fst
            |> List.sort
            |> List.tryHead
        match candidate with
        | Some c -> loop (String.concat "" [sequence; string c])
        | None   -> sequence
    loop ""

let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\7.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (is, _, _) -> order is
    | Failure (err,_, _) -> raise(new Exception(err))
