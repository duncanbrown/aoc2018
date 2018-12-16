// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    DaySixteen.part1 ()
        |> printfn "%A"
    Console.ReadLine () |> ignore
    0 // return an integer exit code
