module DayTwentyOne

open FParsec
open System
open System.Diagnostics

let runProgram (program: DaySixteen.Instruction[]) ip =
    let registers = Array.zeroCreate 6
    registers.[0] <- 1797184L
    let rec loop i n =
        if i = 28 then Debugger.Break()
        let instruction = program.[i]
        //printf "%A %A" instruction registers

        registers.[ip] <- (int64 i)
        DaySixteen.execute registers instruction
        let i' = int ( registers.[ip])
        //printfn " %A"  registers
        if (i'+1) >= program.Length then n
        else loop (i'+1) (n+1)
    loop 0 ip

let part1 () =
    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\21.txt"
    match run DayNineteen.inputParser input with
    | Success ((ip, program),_,_) ->
        runProgram (Array.ofList program) ip
        //Seq.initInfinite id
        //|> Seq.map (runProgram (Array.ofList program) ip)
        //|> Seq.pick id
    | Failure (err,_, _) -> raise(new Exception(err))

