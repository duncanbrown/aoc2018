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


let part1 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\1.txt"
    let parserResult = run inputParser input
    match parserResult with
    | Success (program, _, _) -> calculator 0L program
    | Failure (err,_, _) -> raise(new Exception(err))