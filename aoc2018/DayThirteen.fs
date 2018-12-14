module DayThirteen

open System
open System.Threading


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

let tick (tiles: Tile[][]) carts =
    //Thread.Sleep(1000) 
    //Console.Clear()
    //tiles
    //|> Array.iteri (fun rowi row ->
    //    row
    //    |> Array.iteri (fun coli cell ->
    //        let c = 
    //            match carts |> List.tryFind (fun (p,_,_) -> p = (coli,rowi)) with
    //            | Some (_,f,_) ->
    //                match f with
    //                | Up -> '^'
    //                | Down -> 'v'
    //                | Left -> '<'
    //                | Right -> '>'
    //            | None ->
    //                match cell with
    //                | Straight -> '-'
    //                | TurnRight -> '/'
    //                | TurnLeft -> '\\'
    //                | Intersection -> '+'
    //                | Empty -> ' '
    //        printf "%c" c)
    //    printfn "")
    let cartsInOrder =
        carts
        |> Seq.sortBy (fun ((x,y), _, _) -> y,x)
        |> List.ofSeq
    let rec loop remaining acc =
        match remaining with        
        | []      -> acc
        | c :: cs ->
            let (x,y), facing, nextTurn = c
            let x',y' =
                match facing with
                | Up    -> x,y-1
                | Down  -> x,y+1
                | Left  -> x-1,y
                | Right -> x+1,y
            let otherCartPositions = List.append acc cs
            let crashCartO =
                otherCartPositions
                |> List.tryFind (fun (p, _,_) -> p = (x',y'))
            match crashCartO with
            | Some crashCart ->
                let acc' = acc |> List.except [crashCart]
                let cs' = cs |> List.except [crashCart]
                loop cs' acc'
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
                let acc' = ((x',y'), facing', nextTurn') :: acc
                loop cs acc'
    loop cartsInOrder []                    

let tickUntilOneLeft tiles carts =
    let rec loop c =
        match tick tiles c with
        | [single]     -> single
        | many         -> loop many
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
//    @"/>-<\  ";
//    @"|   |  ";
//    @"| /<+-\";
//    @"| | | v";
//    @"\>+</ |";
//    @"  |   ^";
//    @"  \<->/"|]

//let input =
//    [|
//    @"/->-\        ";
//    @"|   |  /----\";
//    @"| /-+--+-\  |";
//    @"| | |  | v  |";
//    @"\-+-/  \-+--/";
//    @"    \------/ "|]

let part2 () =
    let tiles, carts = parseInput input
    let pos,_,_ = tickUntilOneLeft tiles carts
    pos
    
