module DayTen

open FParsec
open System


let input = "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>"

let parser =
    let pPair =
        spaces >>. pint32 .>> pchar ',' .>> spaces .>>. pint32
    let parseLine =
        pstring "position=<"
        >>. pPair
        .>> pstring "> velocity=<"
        .>>. pPair
        .>> pstring ">"
        .>> spaces
    many parseLine

let bounds points =
    points |> Seq.map fst |> Seq.min,
    points |> Seq.map snd |> Seq.min,
    points |> Seq.map fst |> Seq.max,
    points |> Seq.map snd |> Seq.max

let tick pointsAndVelocities =
    pointsAndVelocities
    |> List.map (fun ((x,y), (dx,dy)) ->
        (x+dx,y+dy),(dx,dy))

let tickUntilClose pointsAndVelocities =
    let rec loop pv i =
        let (minX, minY, maxX, maxY) =
            pv |> List.map fst |> bounds
        if (maxX - minX) < 100 && (maxY - minY) < 100
        then pv, i
        else
            loop (tick pv) (i+1)
    loop pointsAndVelocities 0



let printPoints points =
    let (minX, minY, maxX, maxY) =
        bounds points

    let grid = [
        for y in minY .. maxY ->
        [for x in minX .. maxX ->
            let p = x,y
            if Seq.contains p points
            then '#'
            else '.']
    ]

    grid |> List.iter (fun row ->
        row |> List.iter (printf "%c")
        printfn ""
    )

let runSimulation pointsAndVelocities =
    let rec loop pv i =
        printfn "%i" i
        printPoints (List.map fst pv)
        Console.ReadKey () |> ignore
        loop (tick pv) (i+1)
    let closePoints, closeTicks = tickUntilClose pointsAndVelocities
    loop closePoints closeTicks



let part1 () =
    let parserResult = run parser (System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\10.txt")
    match parserResult with
    | Success (inputData, _, _) -> runSimulation inputData
    | Failure (err,_, _) -> raise(new Exception(err))