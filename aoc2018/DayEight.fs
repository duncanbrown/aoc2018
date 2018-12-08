module DayEight

open System.IO

type Node =
    Node of char * int [] * Node list


let rec getMetadata (node: Node) =
    let (Node (_, metadata, children)) = node
    let childMetadata = List.map getMetadata children |> List.concat
    List.append (List.ofArray metadata) childMetadata

let rec getValue node =
    let (Node (_, metadata, children)) = node
    if children.IsEmpty
    then
        Array.sum metadata
    else
        metadata
            |> List.ofArray
            |> List.choose (fun m -> List.tryItem (m-1) children)
            |> List.sumBy getValue

let trimS (s: string) = s.Trim ()
let splitS c (s: string) = s.Split ([|c|])

let buildTree input =
    let inputNs = input |> trimS |> splitS ' ' |> Array.map int

    let rec readNode index id =
        let childCount = inputNs.[index]
        let metadataCount = inputNs.[index+1]
        let rec readChildren current i =
            if List.length current = childCount
            then current, i
            else
                let nextId = char ( (int id) + 1 + (List.length current))
                let nextChild, newI = readNode i nextId
                readChildren (List.append current [nextChild]) newI
        let children, readUpTo = readChildren [] (index+2)
        let metadata = inputNs.[readUpTo .. readUpTo + metadataCount - 1]
        Node (id, metadata, children), readUpTo + metadataCount

    let rootNode, _ = readNode 0 'A'
    rootNode

let licenceSum input =
    input
    |> buildTree
    |> getMetadata
    |> List.sum

let value input =
    input
    |> buildTree
    |> getValue

let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\8.txt"
    licenceSum input

let part2 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\8.txt"
    value input