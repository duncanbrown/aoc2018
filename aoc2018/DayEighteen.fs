module DayEighteen

open System.Xml.Linq
open System

type Acre =
      Open
    | Trees
    | Lumberyard

type World = Acre[][]

let charsToString (cs: seq<char>) =
    System.String.Join("", cs)

let print (world: World) =
    let toString =
        function
        | Open -> '.'
        | Trees -> '|'
        | Lumberyard -> '#'
    world
    |> Array.iter (fun row ->
        row
        |> Array.map toString
        |> charsToString
        |> printfn "%s")
    printfn ""

let isOpen =
    function
    | Open -> true
    | _ -> false
let isTrees =
    function
    | Trees -> true
    | _ -> false
let islumberyard =
    function
    | Lumberyard -> true
    | _ -> false

let countWhere f xs =
    xs |> Seq.filter f |> Seq.length

let tick (world: World) =
    //print world
    let tick (x,y) acre =
        let adjacents =
            [x-1,y-1;x,y-1;x+1,y-1;
             x-1,y;        x+1,y;
             x-1,y+1;x,y+1;x+1,y+1]
             |> List.filter (fun (x',y') -> x' >=0 && x'< world.[0].Length && y' >= 0 && y' < world.Length)
             |> List.map (fun (x',y') -> world.[y'].[x'])
        match acre with
        | Open ->
            if (countWhere isTrees adjacents) >= 3 then Trees else Open
        | Trees ->
            if (countWhere islumberyard adjacents) >= 3 then Lumberyard else Trees
        | Lumberyard ->
            if (countWhere islumberyard adjacents) >= 1 && (countWhere isTrees adjacents) >= 1
            then Lumberyard
            else Open

    world
    |> Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x acre -> tick (x,y) acre))
        
    
let run world iterations =
    let rec loop w i =
        let w' = tick w
        let i' = i+1
        if i' = iterations then w' else loop w' i'
    loop world 0

let buildWorld inputLines =
    inputLines
    |> Seq.map (fun line ->
        line
        |> Seq.map
            (function
            | '.' -> Open
            | '#' -> Lumberyard
            | '|' -> Trees)
        |> Array.ofSeq)
    |> Array.ofSeq

let part1 () =
    let input = @".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."
    //let inputLines = input.Split(Environment.NewLine) |> Array.map (fun s -> s.Trim())
    let inputLines = System.IO.File.ReadAllLines "c:\\dev\\aoc2018\\input\\18.txt" |> Array.map (fun s -> s.Trim())
    let world = buildWorld inputLines
    let w = run world 10
    print w
    let trees = 
        w |> Array.sumBy (fun row ->
            row
            |> Array.sumBy
                (function
                 | Trees -> 1
                 | _ -> 0))
    let lumberyards = 
        w |> Array.sumBy (fun row ->
            row
            |> Array.sumBy
                (function
                 | Lumberyard -> 1
                 | _ -> 0))
    trees * lumberyards
    