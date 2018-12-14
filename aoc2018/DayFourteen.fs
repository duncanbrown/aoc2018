module DayFourteen

open System
open System.Collections.Generic

let getNext10 after =
    let scoreboard = List(after+11)
    scoreboard.AddRange([3;7])
    let rec loop elf1i elf2i =
        if scoreboard.Count >= (after+10)
        then
            scoreboard.GetRange(after, 10)
        else
            let elf1R = scoreboard.[elf1i]
            let elf2R = scoreboard.[elf2i]
            let newRecipies =
                elf1R + elf2R
                |> string
                |> Seq.map (string >> int)
                |> Array.ofSeq
            scoreboard.AddRange(newRecipies)
            let getNexti i r =
                (i + 1 + r) % scoreboard.Count
            let elf1i' = getNexti elf1i elf1R
            let elf2i' = getNexti elf2i elf2R
            loop elf1i' elf2i'
    loop 0 1

let part1 () =
    getNext10 556061
    |> fun ns -> (System.String.Join(' ',ns))