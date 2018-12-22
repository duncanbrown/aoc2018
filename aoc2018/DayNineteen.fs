module DayNineteen

open DaySixteen
open FParsec
open System

let runProgram (program: Instruction[]) ipRegister =
    let registers = Array.zeroCreate 6
    registers.[0] <- 1L
    //registers.[1] <- reg1Val
    //registers.[2] <- 10551424L
    //registers.[3] <- 2L
    //registers.[4] <- 1L
    //registers.[5] <- 10551400L


    let rec loop i previous =
        let previous' = ((i, Array.copy registers) :: previous)

        let inline normalLoop () = 
            let instruction = program.[i]
            registers.[ipRegister] <- (int64 i)
            execute registers instruction
            let i' = int ( registers.[ipRegister])
            if (i'+1) >= program.Length then registers.[0]
            else loop (i'+1) previous'
        //printfn "%i %A" i registers
        //normalLoop ()
        let p =
            previous
            |> List.tryFind
                (fun (i',r) ->
                    i' = i &&
                    (Array.zip r registers)
                    |> Array.mapi (fun j (p, n) -> j,p,n)
                    |> Array.filter (fun (j,p,n) -> p <> n)
                    |> Array.length = 1)
        if p.IsSome
        then
            //printfn "%A" p
            let (j, p, n) = 
                p.Value
                |> snd
                |> Array.mapi (fun j p -> j,p,registers.[j])
                |> Array.find (fun (j,p,n) -> p <> n)
            if j = 5 && i = 3 // && registers.[5] < registers.[2] / registers.[1]
            then
                //printfn "%i %A" i registers
                registers.[5] <- if registers.[5] > registers.[2] / registers.[1] then registers.[2] else registers.[2] / registers.[1]
                //printfn "%A" registers
                loop i [i, Array.copy registers]//((i, Array.copy registers) :: previous)
            else
                normalLoop ()
                //raise (Exception())
        else
            normalLoop ()
            
    loop 0 []

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
        //Seq.initInfinite id
        //|> Seq.map (runProgram (Array.ofList program) ip)
        //|> Seq.pick id
    | Failure (err,_, _) -> raise(new Exception(err))


