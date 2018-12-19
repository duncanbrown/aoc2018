module DayNineteen

open DaySixteen
open FParsec
open System

let runProgram (program: Instruction[]) ipRegister =
    let registers = Array.zeroCreate 6
    let rec loop i =
        let instruction = program.[i]
        registers.[ipRegister] <- i
        execute registers instruction
        let i' = registers.[ipRegister]
        if (i'+1) >= program.Length then registers.[0]
        else loop (i'+1)
    loop 0

let inputParser =
    let pIp = 
        pstring "#ip " >>. pint32
    let pInstruction =
        pipe4
            (anyString 4
                |>> function
                    | "addr" -> Addr
                    | "addi" -> Addi
                    | "mulr" -> Mulr
                    | "muli" -> Muli
                    | "banr" -> Banr
                    | "bani" -> Bani
                    | "borr" -> Borr
                    | "bori" -> Bori
                    | "setr" -> Setr
                    | "seti" -> Seti
                    | "gtir" -> Gtir
                    | "gtri" -> Gtri
                    | "gtrr" -> Gtrr
                    | "eqir" -> Eqir
                    | "eqri" -> Eqri
                    | "eqrr" -> Eqrr)
             (spaces >>. pint32)
             (spaces >>. pint32)
             (spaces >>. pint32)
             (fun o a b c -> o,a,b,c)
        .>> spaces
    pIp .>> spaces .>>. (many pInstruction)



let part1 () =
//    let input = @"#ip 0
//seti 5 0 1
//seti 6 0 2
//addi 0 1 0
//addr 1 2 3
//setr 1 0 0
//seti 8 0 4
//seti 9 0 5"
    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\19.txt"
    match run inputParser input with
    | Success ((ip, program),_,_) ->
        runProgram (Array.ofList program) ip
    | Failure (err,_, _) -> raise(new Exception(err))


