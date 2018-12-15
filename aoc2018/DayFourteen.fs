module DayFourteen

open System
open System.Collections.Generic

let getNumberToLeftOf (recipe: string) =
    let scoreboard = List(2107929416)
    scoreboard.AddRange([3;7])
    let rec loop elf1i elf2i =
        let startIndexOfRecipe =
            [1;2;3]
            |> List.filter (fun i -> scoreboard.Count > (recipe.Length + i))
            |> List.map (fun i -> scoreboard.Count - recipe.Length - i)
            |> List.map (fun i -> i, scoreboard.GetRange(i, recipe.Length))
            |> List.tryFind (fun (_, ns) ->
                ns
                |> Seq.map string
                |> String.concat "" = recipe)
            |> Option.map fst
        match startIndexOfRecipe with
        | Some i -> i
        | None ->
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

let part2 () =
    getNumberToLeftOf "556061"