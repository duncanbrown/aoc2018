module DayNine

let insertAfter x i (xs: 'T list) =
    List.concat [ xs.[0..i]; [x]; xs.[i+1..] ]

let replace x i (xs: 'T list) =
    List.concat [ xs.[0..i-1]; [x]; xs.[i+1..] ]

let removeAt i (xs: 'T list) =
    xs.[i], List.concat [ xs.[0..i-1]; xs.[i+1..] ]

let modulo n m = ((n % m) + m) % m;;

let getIndexToInsertAfter circle currentMarble =
    let currentMarbleIndex = List.findIndex (fun m -> m = currentMarble) circle
    let length = List.length circle
    if length = 0 then 0
    else
        (currentMarbleIndex + 1) % length
    
let getIndexOfMarbleToRemove circle currentMarble =
    let currentMarbleIndex = List.findIndex (fun m -> m = currentMarble) circle
    let length = List.length circle
    modulo (currentMarbleIndex - 7) length


let score players lastMarble =
    let marbles = { 1 .. lastMarble }
    let initialScores = List.replicate players 0
    let initialCircle = [0]
    let folder (scores: int list, circle, currentMarble, playerIndex) marble =
        let playerIndex' = (playerIndex + 1 ) % List.length scores
        if marble % 23 <> 0
        then
            let indexToAddMarble = getIndexToInsertAfter circle currentMarble
            let circle' = insertAfter marble indexToAddMarble circle
            (scores, circle', marble, playerIndex')
        else
            let indexOfMarbleToRemove = getIndexOfMarbleToRemove circle currentMarble
            let removed, circle' = removeAt indexOfMarbleToRemove circle
            let score = scores.[playerIndex]
            let score' = score + marble + removed
            let scores' = replace score' playerIndex scores
            let marble' = circle.[indexOfMarbleToRemove + 1]
            (scores', circle', marble', playerIndex')
    let finalScores, _, _, _ =
        marbles
        |> Seq.fold folder (initialScores, initialCircle, 0, 0)
    List.max finalScores

let part1 () =
    score 428 70825