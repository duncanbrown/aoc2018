module DayEight

open System.IO

type Node =
    Node of int [] * Node list

let getData (node: Node) =
    match node with
    | Node (m,c) -> m,c

let rec getMetadata (node: Node) =
    let (metadata, children) = getData node
    let childMetadata = List.map getMetadata children |> List.concat
    List.append (List.ofArray metadata) childMetadata

    

let trimS (s: string) = s.Trim ()
let splitS c (s: string) = s.Split ([|c|])

let licenceSum (input: string) =
    let inputNs = input |> trimS |> splitS ' ' |> Array.map int

    let rec readNode index =
        let childCount = inputNs.[index]
        let metadataCount = inputNs.[index+1]
        let rec readChildren current i =
            if List.length current = childCount
            then current, i
            else
                let nextChild, newI = readNode i
                readChildren (nextChild :: current) newI
        let children, readUpTo = readChildren [] (index+2)
        let metadata = inputNs.[readUpTo .. readUpTo + metadataCount - 1]
        Node (metadata, children), readUpTo + metadataCount

    let rootNode, _ = readNode 0
    let metadataSum =
        getMetadata rootNode
        |> List.sum
    metadataSum


let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\8.txt"
    licenceSum input
