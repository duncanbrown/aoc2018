﻿// Learn more about F# at http://fsharp.org

open System
open System

[<EntryPoint>]
let main argv =
    DayOne.part2 ()
        |> printfn "%i"
    Console.ReadLine () |> ignore
    0 // return an integer exit code
