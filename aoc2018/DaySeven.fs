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

let concat s s' =
    String.concat "" [s;s']

let charsToString cs =
    String(Array.ofSeq cs)

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

let duration workers timeModifier instructions =
    let stepTimes =
        ['A'..'Z']
        |> List.mapi (fun i c -> c, i + 1 + timeModifier )
        |> Map.ofList
    let timeForStep c =
        Map.find c stepTimes
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

    let rec tick elapsed workerStates sequence =
        let elapsed' = elapsed + 1
        let workerStates', stepsCompleted, stepsInProgress = 
            workerStates
            |> List.fold
                (fun (states, completed, inProgress) currentWorkerState ->                    
                    match currentWorkerState with
                    | Some c, Some t ->
                        let t' = t - 1
                        if t' > 0
                        then
                            let state = Some c, Some t'
                            let states' = state :: states
                            states', completed, c :: inProgress
                        else
                            let state = None, None
                            let states' = state :: states
                            states', c :: completed, inProgress
                    | None, None  ->
                        currentWorkerState :: states, completed, inProgress)
                ([], [], [])

        let sequence' = concat sequence (charsToString stepsCompleted)

        let nextWork =
            allRequisites
            |> Map.filter (fun k _ -> not (contains k sequence'))
            |> Map.filter (fun _ rs -> Set.forall (fun r -> contains r sequence') rs)
            |> Map.toList
            |> List.map fst
            |> List.except stepsInProgress
            |> List.sort

        if nextWork.IsEmpty && List.forall (fun (c: char option, _) -> c.IsNone) workerStates'
        then elapsed'
        else
            let workerStates'' =
                workerStates'
                |> List.fold
                    (fun (states, work) currentWorkerState ->
                        let _, step = currentWorkerState
                        match work, step with
                        | c::cs, None ->
                            let newState = Some c, Some (timeForStep c)
                            newState :: states , cs
                        | _ ->
                            currentWorkerState :: states, work
                        )
                    ([], nextWork)
                |> fst

            tick elapsed' workerStates'' sequence'

    tick -1 (List.replicate workers (None, None)) ""

let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\7.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (is, _, _) -> order is
    | Failure (err,_, _) -> raise(new Exception(err))

let part2 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\7.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (is, _, _) -> duration 5 60 is
    | Failure (err,_, _) -> raise(new Exception(err))
