module DayTwelve

open System
open DayEight

type RuleBook = Map<(bool * bool * bool * bool * bool), bool>

let get state index =
    if index < 0 || index >= Array.length state
    then false
    else state.[index]

let survives (rules: RuleBook) index (state: bool[]) =
    rules
    |> Map.find (
        get state (index-2),
        get state (index-1),
        get state index,
        get state (index+1),
        get state (index+2) )

let tick rules state =
    Array.mapi (fun i _ -> survives rules i state) state

let aliveAfter generations initialState rules =
    let rec loop generation state =
        if generation = generations
        then state
        else loop (generation+1) (tick rules state)
    let padding = 100
    let padded = Array.concat [ Array.replicate padding false; initialState; Array.replicate padding false ]
    let finalState = loop 0 padded
    let aliveIndices =
        Seq.zip
            (Seq.initInfinite (fun i -> i-padding))
            finalState
        |> Seq.filter snd
        |> Seq.map fst
        |> List.ofSeq
    List.sum aliveIndices

let parseInitialState stateString =
    stateString
    |> Seq.map (fun c -> c = '#')
    |> Array.ofSeq

let parseRules ruleStrings =
    let parseRule (ruleString: string) =
         (ruleString.[0] = '#',
          ruleString.[1] = '#',
          ruleString.[2] = '#',
          ruleString.[3] = '#',
          ruleString.[4] = '#'),
          ruleString.[9] = '#'
    let rules = Seq.map (trimS >> parseRule) ruleStrings
    Map.ofSeq rules

let part1 () =
    let initialState = parseInitialState "##.####..####...#.####..##.#..##..#####.##.#..#...#.###.###....####.###...##..#...##.#.#...##.##.."
    let ruleBook =
        "##.## => #
        ....# => .
        .#.#. => #
        ..### => .
        ##... => #
        ##### => .
        ###.# => #
        .##.. => .
        ..##. => .
        ...## => #
        ####. => .
        ###.. => .
        .#### => #
        #...# => #
        ..... => .
        ..#.. => .
        #..## => .
        #.#.# => #
        .#.## => #
        .###. => .
        ##..# => .
        .#... => #
        .#..# => #
        ...#. => .
        #.#.. => .
        #.... => .
        ##.#. => .
        #.### => .
        .##.# => .
        #..#. => #
        ..#.# => .
        #.##. => #".Split (Environment.NewLine)
        |> parseRules
    aliveAfter 20 initialState ruleBook