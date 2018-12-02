// Learn more about F# at http://fsharp.org

open System
open System

[<EntryPoint>]
let main argv =
    DayTwo.part2 ()
        |> printfn "%s"
    Console.ReadLine () |> ignore
    0 // return an integer exit code
