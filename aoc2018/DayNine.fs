module DayNine

open System
open System.Collections.Generic

let insertAfter x i (xs: List<int>)  =
    xs.Insert (i+1, x)
    //xs
    //let xs' = Array.zeroCreate (xs.Length + 1)
    //Array.blit xs 0 xs' 0 (i+1)
    //Array.set xs' (i+1) x
    //Array.blit xs (i+1) xs' (i+2) (xs.Length - i - 1)
    //xs'

let replace x i (xs: 'T[]) =
    let xs' = Array.copy xs
    Array.set xs' i x
    xs'    

let removeAt i (xs: List<int>) =
    let x = xs.[i]
    xs.RemoveAt (i)
    x
    //let x = xs.[i]
    //let xs' = Array.zeroCreate (xs.Length - 1)
    //Array.blit xs 0 xs' 0 i
    //Array.blit xs (i+1) xs' i (xs.Length - i - 1)
    //x, xs'

let modulo n m = ((n % m) + m) % m;;

let getIndexToInsertAfter (circle: List<int>) currentMarbleIndex =
    let length  = circle.Count
    if length = 0 then 0
    else
        (currentMarbleIndex + 1) % length
    
let getIndexOfMarbleToRemove (circle: List<int>) currentMarbleIndex =
    let length = circle.Count
    modulo (currentMarbleIndex - 7) length


let score players lastMarble =
    let marbles = { 1 .. lastMarble }
    let initialScores = Array.replicate players 0L
    let initialCircle = List<int> (lastMarble + 1)
    initialCircle.Add (0)
    let folder (scores: int64[], circle: List<int>, currentMarble, currentMarbleIndex, playerIndex) marble =
        if marble % 1000 = 0
        then printfn "%i" marble
        else ()
        let playerIndex' = (playerIndex + 1 ) % Array.length scores
        if marble % 23 <> 0
        then
            let indexToAddMarbleAfter = getIndexToInsertAfter circle currentMarbleIndex
            insertAfter marble indexToAddMarbleAfter circle
            (scores, circle, marble, indexToAddMarbleAfter+1, playerIndex')
        else
            let indexOfMarbleToRemove = getIndexOfMarbleToRemove circle currentMarbleIndex
            let removed = removeAt indexOfMarbleToRemove circle
            let score = scores.[playerIndex]
            let score' = score + (int64 marble) + (int64 removed)
            let scores' = replace score' playerIndex scores
            let marble' = circle.[indexOfMarbleToRemove]
            (scores', circle, marble', indexOfMarbleToRemove, playerIndex')
    let finalScores, _, _, _, _ =
        marbles
        |> Seq.fold folder (initialScores, initialCircle, 0, 0, 0)
    Array.max finalScores

let part1 () =
    score 428 70825

let part2 () =
    score 428 7082500