module DaySeventeen

open FParsec
open System

type Cell =
    Clay | Sand | WaterAtRest | WaterHasPassed | Tap

let toString = function
    | Clay -> '#'
    | Sand -> '.'
    | WaterAtRest -> '~'
    | WaterHasPassed -> '|'
    | Tap -> '+'

let print (world: Cell[][]) =    
    world
    |> Array.take 100
    |> Array.iter (fun row ->
        printfn "%s" (System.String.Join("",row |> Array.skip 400 |> Array.take 100 |> Array.map toString))
        )

let inputParser =
    let pLine =
        parse {
            let! var1 = anyChar
            do! skipString "="
            let! var1ValStart, var1ValEndO =
                pint32
                .>> (opt (pstring ".."))
                .>>. (opt pint32)
            do! skipString ", "
            let! var2 = anyChar
            do! skipString "="
            let! var2ValStart, var2ValEndO =
                pint32
                .>> (opt (pstring ".."))
                .>>. (opt pint32)
            let var1Val = [var1ValStart..(Option.defaultValue var1ValStart var1ValEndO)]
            let var2Val = [var2ValStart..(Option.defaultValue var2ValStart var2ValEndO)]
            do! spaces
            let x,y =
                if var1 = 'x'
                then var1Val, var2Val
                else var2Val, var1Val

            return x,y
        }
    many pLine

let buildWorld input =
    match run inputParser input with
    | Success (data,_,_) ->
        let maxX =
            data |> List.map fst |> List.concat |> List.max
        let maxY =
            data |> List.map snd |> List.concat |> List.max
        let minY =
            data |> List.map snd |> List.concat |> List.min
        let world =
            Array.init (maxY + 2)
                (fun _ -> Array.create (maxX + 2) Sand)
        data
        |> List.iter (fun clay ->
            let xs,ys = clay
            List.allPairs xs ys
            |> List.iter (fun (x,y) -> world.[y].[x] <- Clay)
            )
        world.[0].[500] <- Tap
        world, minY, maxY
    | Failure (err,_, _) -> raise(new Exception(err))

let canTakeWater (world: Cell[][]) (x,y) =
    world.[y].[x] = Sand || world.[y].[x] = WaterHasPassed

let waterCanReach world minY maxY = 
        world
        |> Array.skip minY
        |> Array.take (maxY-minY+1)
        |> Array.sumBy
            (fun row -> row |> Array.sumBy (fun cell ->
                match cell with
                //| WaterHasPassed -> 1
                | WaterAtRest -> 1
                | _ -> 0))


let fill (world: Cell[][]) minY maxY =
    let rec loop (startPoints: (int * int) list) (x,y) i =
        if i % 10000 = 0 then printfn "%i" (waterCanReach world minY maxY)
        //print world
        //Console.ReadKey()
        if false then () // (world.[y].[x] = WaterAtRest) then loop (500,0) (500,0)
        else
            if y > (Array.length world - 2)// || (world.[y].[x] = WaterAtRest)
            then ()
            else        
                if canTakeWater world (x,y+1)
                then
                    world.[y].[x] <- WaterHasPassed
                    loop startPoints (x,y+1) (i+1)
                else
                
                    let canSettleToLeft =
                        let testCell =
                            (Seq.initInfinite (fun i -> x-i)
                            |> Seq.takeWhile (fun x' ->
                                canTakeWater world (x',y) && not (canTakeWater world (x',y+1)))
                            |> Seq.last) - 1, y
                        not (canTakeWater world testCell)
                    let canSettleToRight =
                        let testCell =
                            (Seq.initInfinite (fun i -> x+i)
                            |> Seq.takeWhile (fun x' -> canTakeWater world (x',y) && not (canTakeWater world (x',y+1)))
                            |> Seq.last) + 1, y
                        not (canTakeWater world testCell)
                    if canSettleToLeft && canSettleToRight
                    then
                        let fillToLeft =
                            Seq.initInfinite (fun i -> x-i)
                            |> Seq.takeWhile (fun x' -> canTakeWater world (x',y))
                            |> Seq.last
                        let fillToRight =
                            Seq.initInfinite (fun i -> x+i)
                            |> Seq.takeWhile (fun x' -> canTakeWater world (x',y))
                            |> Seq.last
                        [fillToLeft..fillToRight]
                        |> List.iter (fun x' -> world.[y].[x'] <- WaterAtRest)
                        let startPoints' = startPoints |> List.skipWhile (fun (spx,spy) -> spy = y)
                        loop startPoints' (List.head startPoints') (i+1)
                    else
                        let fillToLeft =
                            Seq.initInfinite (fun i -> x-i)
                            |> Seq.takeWhile (fun x' -> canTakeWater world (x',y) && not (canTakeWater world (x',y+1)))
                            |> Seq.last
                        let fillToRight =
                            Seq.initInfinite (fun i -> x+i)
                            |> Seq.takeWhile (fun x' -> canTakeWater world (x',y) && not (canTakeWater world (x',y+1)))
                            |> Seq.last
                        [fillToLeft..fillToRight]
                        |> List.iter (fun x' -> world.[y].[x'] <- WaterHasPassed)
                        let dripFromLeft = fillToLeft-1,y
                        if canTakeWater world dripFromLeft
                        then
                            loop (dripFromLeft :: startPoints) dripFromLeft (i+1)
                        let dripFromRight = fillToRight+1,y
                        if canTakeWater world dripFromRight
                        then
                            loop (dripFromRight :: startPoints) dripFromRight (i+1)
                        //print world
                        ()                    
                    
    loop [(500,0)] (500,0) 1


let part1 () =
//    let input = @"x=495, y=2..7
//y=7, x=495..501
//x=501, y=3..7
//x=498, y=2..4
//x=506, y=1..2
//x=498, y=10..13
//x=504, y=10..13
//y=13, x=498..504"
    let input = System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\17.txt"
    let world, minY, maxY = buildWorld input
    fill world minY maxY
    waterCanReach world minY maxY