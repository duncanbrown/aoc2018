// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    DayEleven.part2 ()
        |> printfn "%A"
    Console.ReadLine () |> ignore
    0 // return an integer exit code
