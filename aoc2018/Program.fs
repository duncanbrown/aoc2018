﻿// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    DayEight.part2 ()
        |> printfn "%i"
    Console.ReadLine () |> ignore
    0 // return an integer exit code
