module DayTwenty

open System.Text
open DaySeven
open System.Collections.Generic
open System

let getBranches s =
    let rec loop (s: string) openCount accBranches (acc: StringBuilder) =
        let c = s.[0]
        let cs = s.Substring(1)
        if c = '('
        then loop cs (openCount+1) accBranches (acc.Append(c))
        else if c = '|' && openCount = 0
        then loop cs openCount (acc :: accBranches) (StringBuilder())
        else if c = ')' && openCount = 0
        then
            let branches =
                (acc :: accBranches)
                |> List.map (fun sb -> sb.ToString())
            branches, cs
        else if c = ')'
        then loop cs (openCount-1) accBranches (acc.Append(c))
        else loop cs openCount accBranches (acc.Append(c))
    let branches, remaining = loop s 0 [] (StringBuilder())
    let branches' = branches |> List.filter (fun b -> String.length b > 0)
    branches', remaining

let getEndpoint (s: string) startPoint =
    s
    |> Seq.fold
        (fun (x,y) c ->
            match c with
            | 'N' -> x,y-1
            | 'S' -> x,y+1
            | 'E' -> x+1,y
            | 'W' -> x-1,y)
        startPoint
let substrings s = s |> Seq.scan (fun acc next -> acc + (string next)) "" |> List.ofSeq

type Tree<'a> = Tree of 'a * Tree<'a> list
let rec buildTree s = 
    if String.length s = 0
    then []
    else
        let branchless = s |> Seq.takeWhile (fun c -> c <> '(') |> charsToString
        let afterBranchless = s.Substring(branchless.Length)
        if String.length afterBranchless = 0
        then
            //branchless |> substrings |> List.map (fun s -> Tree(s, []))
            [Tree(s,[])]
        else
            let branches, remaining =
                getBranches (afterBranchless.Substring(1))
            if branches |> List.length = 1
            then
                let branch = List.head branches
                let points = substrings branch
                //let furthest = branch.Substring(branch.Length / 2)
                points
                |> List.map (fun s -> buildTree (branchless + s))
                |> List.concat
                |> List.append
                    (buildTree (branchless + remaining))
            else
                let children = branches |> List.map buildTree
                if remaining.Length > 0 then raise (Exception())
                children |> List.map (fun cs -> Tree(branchless, cs))

let processTrees trees =
    let rec processTree parent tree =
        let pos, length  =
            match parent with
            | Some (p,l) -> p,l
            | None       -> (0,0), 0
        let (Tree(data, children)) = tree
        let pos' = getEndpoint data pos
        let length' = length + (String.length data)
        let data' = pos', length'
        let children' = children |> List.map (processTree (Some data'))
        Tree(data', children')
    trees |> List.map (processTree None)

let rec getLeafData tree =
    let (Tree(data, children)) = tree
    if List.length children = 0
    then [data]
    else children |> List.map getLeafData |> List.concat


let part1 () =
    let input =
        (System.IO.File.ReadAllText "c:\\dev\\aoc2018\\input\\20.txt").TrimEnd().Trim([|'^';'$'|])
    let t = buildTree input
    let endPoints = t |> processTrees |> List.map getLeafData |> List.concat
    let g = 
        endPoints
        |> List.groupBy fst
    g |> List.map (fun (pos, paths) -> paths |> List.map snd |> List.min)
    |> List.max
