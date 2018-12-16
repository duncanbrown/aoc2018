module DaySixteen

open FParsec
open System

type OpCode =
    Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr

type Instruction = OpCode * int * int * int

let execute (registers: int[]) = function
    |  Addr, a, b, c ->
        registers.[c] <- registers.[a] + registers.[b]
    |  Addi, a, b, c ->
        registers.[c] <- registers.[a] + b
    |  Mulr, a, b, c ->
        registers.[c] <- registers.[a] * registers.[b]
    |  Muli, a, b, c ->
        registers.[c] <- registers.[a] * b
    |  Banr, a, b, c ->
        registers.[c] <- registers.[a] &&& registers.[b]
    |  Bani, a, b, c ->
        registers.[c] <- registers.[a] &&& b
    |  Borr, a, b, c ->
        registers.[c] <- registers.[a] ||| registers.[b]
    |  Bori, a, b, c ->
        registers.[c] <- registers.[a] ||| b
    |  Setr, a, _, c ->
        registers.[c] <- registers.[a]
    |  Seti, a, _, c ->
        registers.[c] <- a
    |  Gtir, a, b, c ->
        registers.[c] <- if a > registers.[b] then 1 else 0
    |  Gtri, a, b, c ->
        registers.[c] <- if registers.[a] > b then 1 else 0
    |  Gtrr, a, b, c ->
        registers.[c] <- if registers.[a] > registers.[b] then 1 else 0
    |  Eqir, a, b, c ->
        registers.[c] <- if a = registers.[b] then 1 else 0
    |  Eqri, a, b, c ->
        registers.[c] <- if registers.[a] = b then 1 else 0
    |  Eqrr, a, b, c ->
        registers.[c] <- if registers.[a] = registers.[b] then 1 else 0


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

let inputParser =
    let parseSample =
        let parseArray =
            parse {
                do! skipChar '['
                let! a = pint32
                do! skipString ", "
                let! b = pint32
                do! skipString ", "
                let! c = pint32
                do! skipString ", "
                let! d = pint32
                do! skipChar ']'
                do! spaces
                return [|a;b;c;d|]
            }
        parse {
            do! skipString "Before:"
            do! spaces
            let! before = parseArray
            let! instruction = many (pint32 .>> spaces)
            do! skipString "After:"
            do! spaces
            let! after = parseArray
            return before, Array.ofList instruction, after
        }
    many parseSample

let part1 () =
    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\16.txt"
    match run inputParser input with
    | Success (samples,_,_) ->
        samples
        |> List.filter (fun s -> behavesLikeOpCodes s >= 3)
        |> List.length
    | Failure (err,_, _) -> raise(new Exception(err))