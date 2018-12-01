// Learn more about F# at http://fsharp.org

open System
open System

[<EntryPoint>]
let main argv =
    DayOne.part1 ()
        |> printfn "%i"
    Console.ReadLine ()
    0 // return an integer exit code
