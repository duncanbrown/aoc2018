module DayFifteen

open System
open FSharp.Collections.ParallelSeq
open System.Diagnostics

type UnitPoints = { hitPoints: int; attackPower: int }

type Unit =
    Elf of UnitPoints
    | Goblin of UnitPoints

type Cell =
    Wall
    | Open
    | Unit of Unit

type World = Cell[][]

let charsToString cs =
    System.String(Array.ofSeq cs)

let toString = function
| Wall   -> '#'
| Open   -> '.'
| Unit u ->
    match u with
    | Elf _    -> 'E'
    | Goblin _ -> 'G'



let move (world: World) (fromX,fromY) (toX,toY) =
    world.[toY].[toX] <- world.[fromY].[fromX]
    world.[fromY].[fromX] <- Open
    world

let updateUnit (world: World) (x,y) unit =
    let hitPoints =
        match unit with
        | Elf data -> data.hitPoints
        | Goblin data -> data.hitPoints
    if hitPoints > 0
    then
        world.[y].[x] <- Unit unit
    else
        world.[y].[x] <- Open
    world


let getUnitsAndPositions world =
    world
        |> Array.mapi
            (fun y row ->
                row
                |> Array.mapi (fun x c -> x,c)
                |> Array.choose
                    (function
                     | x, Unit u -> Some ((x,y), u)
                     | _, _      -> None))
        |> Array.concat

let getCell (world: World) (x,y) =
    world.[y].[x]

let print (world: World) =
    world
    |> Array.iter (fun row ->
        row |> Array.map toString |> charsToString |> printfn "%s" )
    let units = getUnitsAndPositions world
    units
    |> Array.map snd
    |> Array.iter (function
        | Elf d -> printfn "Elf %i" d.hitPoints
        | Goblin d -> printfn "Goblin %i" d.hitPoints)


let isUnit = function
    | Unit _ -> true
    | _ -> false

let isOppositeUnit from = function
    | Unit u ->
        match from, u with
        | Elf _, Goblin _ -> true
        | Goblin _, Elf _ -> true
        | _ -> false
    | _ -> false
let isOpen = function
    | Open _ -> true
    | _ -> false

let firstByReadingOrder ps =
    ps |> Seq.sortBy (fun (x,y) -> y,x) |> Seq.tryHead

let getAdjacentEnemies world unit (x,y) =
    [x-1,y;x+1,y;x,y-1;x,y+1]
    |> List.map (fun p -> p, getCell world p)
    |> List.filter (fun (_, cell) -> isOppositeUnit unit cell)

type Move =
    StayAndFight
    | MoveTo of int * int
    | CantMove

type PathInfo = { firstStep: int * int; lastStep: int * int }

let getNextMove world unit pos =
    let rec buildPaths (paths: (int * int) list list) positionsPassed =
        let getSteps path =
            let (x,y) :: _ = path
            let candidates =
                [x-1,y;x+1,y;x,y-1;x,y+1]
                |> List.except [pos]
                |> List.map (fun p -> p, getCell world p)
            if List.exists (fun (_,cell) -> isOppositeUnit unit cell) candidates
            then Choice1Of2 ()
            else
                candidates
                |> Seq.filter (fun (_,c) -> isOpen c)
                |> Seq.filter (fun (p,_) -> not (Set.contains p positionsPassed))
                |> Seq.map fst
                |> List.ofSeq
                |> Choice2Of2
        let pathsAndNextSteps = paths |> PSeq.map (fun p -> p, getSteps p) |> List.ofSeq
        let pathsAlreadyAtEnemies =
            pathsAndNextSteps
            |> List.filter ( fun (_, nextSteps) ->
                match nextSteps with
                | Choice1Of2 _ -> true
                | _ -> false)
            |> List.map fst
        if not (List.isEmpty pathsAlreadyAtEnemies)
        then
            let positionOfEnemyToTarget =
                pathsAlreadyAtEnemies
                |> List.map (fun p ->
                    let endOfPath :: _ = p
                    getAdjacentEnemies world unit endOfPath
                    |> List.map fst
                    |> firstByReadingOrder
                    |> Option.get)
                |> firstByReadingOrder
                |> Option.get
            let pathsLeadingToEnemyToTarget =
                pathsAlreadyAtEnemies
                |> List.filter (fun p -> 
                    let endOfPath :: _  = p
                    getAdjacentEnemies world unit endOfPath
                    |> List.map fst
                    |> List.contains positionOfEnemyToTarget)
            let firstStepsOfPathsLeadingToEnemyToTarget =
                pathsLeadingToEnemyToTarget
                |> List.map (fun path ->
                    if List.length path = 1 then path.[0] else path.[path.Length - 2])
            Some (firstByReadingOrder firstStepsOfPathsLeadingToEnemyToTarget |> Option.get)
        else          
            let pathsAndNextStepsUnwrapped =
                pathsAndNextSteps
                |> List.map (fun (p,stepsC) ->
                    let (Choice2Of2 steps) = stepsC
                    p, steps)
            let paths' =
                pathsAndNextStepsUnwrapped
                |> Seq.filter (fun (_, steps) -> not (List.isEmpty steps))
                |> Seq.map (fun (p, steps) -> steps |> List.map (fun s -> s :: p)) // if p.lastStep = p.firstStep then { firstStep = s; lastStep = s; } else { p with lastStep = s}))
                |> Seq.concat
                |> List.ofSeq
            if List.isEmpty paths'
            then None
            else
                let newSteps =
                    pathsAndNextStepsUnwrapped
                    |> Seq.map snd
                    |> Seq.concat
                    |> Set.ofSeq
                let positionsPassed' = Set.union positionsPassed newSteps
                buildPaths paths' positionsPassed'
    let stepToTake =
        match buildPaths [[pos]] (Set.singleton pos) with
        | None   -> CantMove
        | Some s ->
            if s = pos then StayAndFight else MoveTo s
    stepToTake
            

let tick unit pos world =
    let unitData =
        match unit with
        | Elf data -> data
        | Goblin data -> data
    let nextMove = getNextMove world unit pos
    let newPos, worldAfterMove =
        match nextMove with
        | MoveTo (x,y) -> (x,y), move world pos (x,y)
        | _ -> pos, world
    let targetPos =
        getAdjacentEnemies worldAfterMove unit newPos
        |> List.sortBy (fun ((x,y), c) ->
            let (Unit u) = c
            match u with
            | Elf d -> d.hitPoints, y, x
            | Goblin d -> d.hitPoints, y, x)
        |> List.tryHead
        |> Option.map fst
    match targetPos with
    | None -> worldAfterMove
    | Some t ->
        let (Unit target) = getCell worldAfterMove t
        let target' =
            match target with
            | Elf data -> Elf { data with hitPoints = data.hitPoints - unitData.attackPower }
            | Goblin data -> Goblin { data with hitPoints = data.hitPoints - unitData.attackPower }
        updateUnit world t target'    
            

let getElfCount units = 
    units
    |> Array.map snd
    |> Array.filter (function
        | Elf _ -> true
        | Goblin _ -> false)
    |> Array.length

let getGoblinCount units = 
    units
    |> Array.map snd
    |> Array.filter (function
        | Elf _ -> true
        | Goblin _ -> false)
    |> Array.length

let tickWorld world =
    //print world
    //Console.ReadLine() |> ignore
    let sw = Stopwatch.StartNew()
    let unitsInOrder = getUnitsAndPositions world
    let elfCount = getElfCount unitsInOrder
    let folder acc (i, (unitPos, unit)) =
        printfn "Unit %i" i
        match getCell acc unitPos with
        | Open   -> acc // Unit has been destroyed
        | Unit _ -> tick unit unitPos acc            
    let r = unitsInOrder |> Array.mapi (fun i u -> i,u) |> Array.fold folder world
    printfn "%A" sw.Elapsed
    r

let run world =
    let initialElfCount = world |> getUnitsAndPositions |> getElfCount
    let rec loop w i =
        printfn "Loop %i" i
        let units = w |> getUnitsAndPositions
        let elfCount = getElfCount units
        if elfCount < initialElfCount then None
        else
            let gameOver =
                units
                |> Array.map snd
                |> Array.distinctBy (fun u -> u.GetType())
                |> Array.length = 1
            if gameOver
            then
                let hitPointsLeft =
                    units
                    |> Array.map snd
                    |> Array.sumBy (function
                        | Elf d -> d.hitPoints
                        | Goblin d -> d.hitPoints)
                Some (hitPointsLeft * (i-1), hitPointsLeft * i)
            else
                loop (tickWorld w) (i+1)
    loop world 0

let parseWorld lines elfHitPower =
    printfn "Elf power: %i" elfHitPower
    let g = Unit (Goblin { hitPoints = 200; attackPower = 3; })
    let e = Unit (Elf { hitPoints = 200; attackPower = elfHitPower; })
    lines
    |> Seq.map (fun (row: string) ->
        row.Trim()
        |> Seq.map (fun c ->
            match c with
            | '#' -> Wall
            | '.' -> Open
            | 'E' -> e
            | 'G' -> g)
        |> Array.ofSeq)
    |> Array.ofSeq

let part1 () =    
    //let input = System.IO.File.ReadAllLines "c:\\dev\\aoc2018\\input\\15.txt"
    let input = @"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"
    run (parseWorld (input.Split(Environment.NewLine)) 15)

let part2 () =
    let sw = Stopwatch.StartNew()
    let input = @"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"
    //let input = System.IO.File.ReadAllLines "c:\\dev\\aoc2018\\input\\15.txt"
    let candidates = Seq.initInfinite id |> Seq.skip 4
    let r =
        candidates
        |> Seq.map (fun p -> p, parseWorld (input.Split(Environment.NewLine)) p |> run)
        |> Seq.pick (fun (p,r) -> r |> Option.map (fun x -> p,x) )
        |> snd
    printfn "%A" sw.Elapsed
    r