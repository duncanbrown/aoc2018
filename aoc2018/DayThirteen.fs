module DayThirteen

open System

type Tile =
      Straight
    | TurnRight
    | TurnLeft
    | Intersection
    | Empty

type Direction =
      Up
    | Down
    | Left
    | Right

type TurnBehaviour =
      GoLeft
    | GoStraight
    | GoRight

let tickOrCrash (tiles: Tile[][]) carts =
    let cartsInOrder =
        carts
        |> Seq.sortBy (fun ((x,y), _, _) -> y,x)
        |> List.ofSeq
    let rec loop remaining acc =
        match remaining with        
        | []      -> Choice1Of2 acc
        | c :: cs ->
            let (x,y), facing, nextTurn = c
            let x',y' =
                match facing with
                | Up    -> x,y-1
                | Down  -> x,y+1
                | Left  -> x-1,y
                | Right -> x+1,y
            let otherCartPositions = List.append acc cs
            let crashPos =
                otherCartPositions
                |> List.tryFind (fun (p, _,_) -> p = (x',y'))
                |> Option.map (fun (p, _,_) -> p)
            match crashPos with
            | Some p -> Choice2Of2 p
            | None ->
                let facing', nextTurn' =
                    match tiles.[y'].[x'] with
                    | Straight ->
                        facing, nextTurn
                    | TurnRight ->
                        match facing with
                        | Up    -> Right, nextTurn
                        | Down  -> Left, nextTurn
                        | Left  -> Down, nextTurn
                        | Right -> Up, nextTurn
                    | TurnLeft ->
                        match facing with
                        | Up    -> Left, nextTurn
                        | Down  -> Right, nextTurn
                        | Left  -> Up, nextTurn
                        | Right -> Down, nextTurn
                    | Intersection ->
                        match nextTurn with
                        | GoLeft ->
                            match facing with
                            | Up    -> Left, GoStraight
                            | Down  -> Right, GoStraight
                            | Left  -> Down, GoStraight
                            | Right -> Up, GoStraight
                        | GoRight ->
                            match facing with
                            | Up    -> Right, GoLeft
                            | Down  -> Left, GoLeft
                            | Left  -> Up, GoLeft
                            | Right -> Down, GoLeft
                        | GoStraight -> facing, GoRight
                loop cs (((x',y'), facing', nextTurn') :: acc)
    loop cartsInOrder []                    

let tickUntilCrash tiles carts =
    let rec loop c =
        match tickOrCrash tiles c with
        | Choice1Of2 c' -> loop c'
        | Choice2Of2 p  -> p
    loop carts


let parseInput lines =
    let parseLine line =
        let tilesAndCarts =
            line            
            |> Seq.map
                (function
                | '/'  -> TurnRight, None
                | '-'  -> Straight, None
                | '\\' -> TurnLeft, None
                | '|'  -> Straight, None
                | '+'  -> Intersection, None
                | '>'  -> Straight, Some Right
                | '<'  -> Straight, Some Left
                | '^'  -> Straight, Some Up
                | 'v'  -> Straight, Some Down
                | ' '  -> Empty, None
                |  c    -> failwith (string c))
            |> Array.ofSeq
        let tiles =
            tilesAndCarts |> Array.map fst
        let carts =
            tilesAndCarts
            |> Array.map snd
            |> Array.mapi (fun i x -> x |> Option.map (fun d -> d,i))
            |> Array.choose id
        tiles, carts
    let allTilesAndCarts = lines |> Array.map parseLine
    let tiles =
        allTilesAndCarts
        |> Array.map fst
    let carts =
        allTilesAndCarts
        |> Array.map snd
        |> Array.mapi (fun row carts ->
            carts |> Array.map (fun (d,col) -> (col,row),d,GoLeft))
        |> Array.concat
        |> List.ofArray
    tiles, carts
            

let input =
    System.IO.File.ReadAllLines "c:\\dev\\aoc2018\\input\\13.txt"


//let input =
//    [|
//    @"/->-\        ";
//    @"|   |  /----\";
//    @"| /-+--+-\  |";
//    @"| | |  | v  |";
//    @"\-+-/  \-+--/";
//    @"    \------/ "|]

let part1 () =
    let tiles, carts = parseInput input
    tickUntilCrash tiles carts
