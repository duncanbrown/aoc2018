module DayTwentyOne

open FParsec
open System
open System.Diagnostics
open FSharp.Collections.ParallelSeq

let runProgram (program: DaySixteen.Instruction[]) ip =
    let registers = Array.zeroCreate 6
    let rec loop i ones =
        let inline doLoop ones' =
            let instruction = program.[i]
            registers.[ip] <- (int64 i)
            DaySixteen.execute registers instruction
            let i' = int ( registers.[ip])
            if (i'+1) >= program.Length then raise (Exception())
            else loop (i'+1) ones'
        if i = 28
        then
            let valueOf1 = registers.[1]
            if ones |> PSeq.exists (fun o -> o = valueOf1)
            then List.head ones
            else doLoop (valueOf1 :: ones)
        else doLoop ones
                
    loop 0 List.empty

let part1 () =
    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\21.txt"
    match run DayNineteen.inputParser input with
    | Success ((ip, program),_,_) ->
        runProgram (Array.ofList program) ip
    | Failure (err,_, _) -> raise(new Exception(err))