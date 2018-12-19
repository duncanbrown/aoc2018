module DaySixteen

open FParsec
open System

type OpCode =
    Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr

type Instruction = OpCode * int * int * int

let execute (registers: int64[]) = function
    |  Addr, a, b, c ->
        registers.[c] <- registers.[a] + registers.[b]
    |  Addi, a, b, c ->
        registers.[c] <- registers.[a] + (int64 b)
    |  Mulr, a, b, c ->
        registers.[c] <- registers.[a] * registers.[b]
    |  Muli, a, b, c ->
        registers.[c] <- registers.[a] * (int64 b)
    |  Banr, a, b, c ->
        registers.[c] <- registers.[a] &&& registers.[b]
    |  Bani, a, b, c ->
        registers.[c] <- registers.[a] &&& (int64 b)
    |  Borr, a, b, c ->
        registers.[c] <- registers.[a] ||| registers.[b]
    |  Bori, a, b, c ->
        registers.[c] <- registers.[a] ||| (int64 b)
    |  Setr, a, _, c ->
        registers.[c] <- registers.[a]
    |  Seti, a, _, c ->
        registers.[c] <- int64 a
    |  Gtir, a, b, c ->
        registers.[c] <- if int64 a > registers.[b] then 1L else 0L
    |  Gtri, a, b, c ->
        registers.[c] <- if registers.[a] > int64 b then 1L else 0L
    |  Gtrr, a, b, c ->
        registers.[c] <- if registers.[a] > registers.[b] then 1L else 0L
    |  Eqir, a, b, c ->
        registers.[c] <- if int64 a = registers.[b] then 1L else 0L
    |  Eqri, a, b, c ->
        registers.[c] <- if registers.[a] = int64 b then 1L else 0L
    |  Eqrr, a, b, c ->
        registers.[c] <- if registers.[a] = registers.[b] then 1L else 0L


let couldBe opCode sample =
    let before, (input: int[]), after = sample
    let rs = Array.copy before
    let instruction = opCode, input.[1], input.[2], input.[3]
    execute rs instruction
    after = rs

let allOpCodes = 
    Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<OpCode>)
    |> Array.map (fun t -> Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(t, [||]) :?>OpCode)

let behavesLikeOpCodes sample =
    allOpCodes
        |> Seq.filter (fun opCode -> couldBe opCode sample)
        |> Seq.length

let determineOpCodes samples =
    let candidatesById =
        samples
        |> Seq.map (fun s ->
            let _,(instruction: int[]),_ = s
            let opCodeId: int = instruction.[0]
            let candidates = 
                allOpCodes
                |> Seq.filter (fun opCode -> couldBe opCode s)
                |> Set.ofSeq
            opCodeId, candidates)
        |> Seq.groupBy fst
        |> Seq.map (fun (id, g) -> id, g |> Seq.map snd |> Set.intersectMany)
        |> List.ofSeq

    let rec findAssignment i assignments =
        if i = List.length candidatesById
        then Some assignments
        else
            let id, candidates = candidatesById.[i]
            let assigned = assignments |> Map.toList |> List.map snd |> Set.ofList
            let candidates' = Set.difference candidates assigned
            if Set.isEmpty candidates'
            then None
            else
                candidates'
                |> Seq.tryPick (fun c ->
                    let assignments' = Map.add id c assignments
                    findAssignment (i+1) assignments')

    match findAssignment 0 Map.empty with
    | Some a -> a
    | None -> raise (Exception())

let runProgram program =
    program
    |> List.fold
        (fun acc next ->
            let registers = Array.copy acc
            execute registers next
            registers)
        [|0L;0L;0L;0L|]

//let inputParser =
//    let parseSample =
//        let parseArray =
//            parse {
//                do! skipChar '['
//                let! a = pint32
//                do! skipString ", "
//                let! b = pint32
//                do! skipString ", "
//                let! c = pint32
//                do! skipString ", "
//                let! d = pint32
//                do! skipChar ']'
//                do! spaces
//                return [|a;b;c;d|]
//            }
//        parse {
//            do! skipString "Before:"
//            do! spaces
//            let! before = parseArray
//            let! instruction = many (pint32 .>> spaces)
//            do! skipString "After:"
//            do! spaces
//            let! after = parseArray
//            return before, Array.ofList instruction, after
//        }
//    let parseInstruction =
//        many1 (pint32 .>> opt (pchar ' ')) .>> spaces
//    (many parseSample)
//        .>> spaces
//        .>>. (many parseInstruction)

//let part1 () =
//    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\16.txt"
//    match run inputParser input with
//    | Success ((samples, program),_,_) ->
//        samples
//        |> List.filter (fun s -> behavesLikeOpCodes s >= 3)
//        |> List.length
//    | Failure (err,_, _) -> raise(new Exception(err))

//let part2 () =
//    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\16.txt"
//    match run inputParser input with
//    | Success ((samples, program),_,_) ->
//        let opCodes = 
//            samples
//            |> determineOpCodes
//        let instructions =
//            program
//            |> List.map (fun ns ->
//                let opCodeId :: abc = ns
//                let opCode = Map.find opCodeId opCodes
//                opCode, abc.[0], abc.[1], abc.[2]
//                )
//        let result = runProgram instructions
//        result.[0]
//    | Failure (err,_, _) -> raise(new Exception(err))