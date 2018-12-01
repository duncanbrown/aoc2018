module DayOne

open FParsec.Primitives
open System
open FParsec
open System.Text
open System.IO

type Operator =
    Plus | Minus

type Expression =
    Operator * int64

type Program =
    Expression list
       
type InputParser = FParsec.Primitives.Parser<Program, unit>

let inputParser: InputParser =
    let parseOperator =
        anyOf "+-" |>> function
        | '+' -> Plus
        | _   -> Minus

    let parseExpression =
        parseOperator .>>. pint64 .>> spaces

    many parseExpression
    
type Calculator =
    int64 -> Program -> int64

let calculator: Calculator =
    let applyExp acc next =
        let op, n = next
        match op with
        | Plus  -> acc + n
        | Minus -> acc - n
    List.fold applyExp

let repeatDetector (program: Program) =
    let length = program.Length
    let getNextIndex currentIndex =
        (currentIndex + 1) % length
    let rec loop frequency frequencies i =
        let op, n = program.[i]
        let nextFrequency =
            match op with
            | Plus  -> frequency + n
            | Minus -> frequency - n
        if Set.contains nextFrequency frequencies
            then nextFrequency
            else loop nextFrequency (Set.add nextFrequency frequencies) (getNextIndex i)
    loop 0L (Set.singleton 0L) 0    
    

let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\1.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (program, _, _) -> calculator 0L program
    | Failure (err,_, _) -> raise(new Exception(err))

let part2 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\1.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (program, _, _) -> repeatDetector program
    | Failure (err,_, _) -> raise(new Exception(err))