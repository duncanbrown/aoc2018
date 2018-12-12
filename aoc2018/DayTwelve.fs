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

let aliveIndicesAfter (generations: int64) initialState rules =
    let padding = 1000       
    
    let rec loop generation state (previousAliveIndices: (int64 * int[]) list) =
        let aliveIndices = state |> Seq.mapi (fun i x -> i-padding,x) |> Seq.filter snd |> Seq.map fst |> Array.ofSeq
        if generation = generations
        then aliveIndices |> Array.map int64
        else            
            let previousMatch =
                previousAliveIndices
                |> List.tryFind (fun (_, is) ->
                    Array.length is = Array.length aliveIndices &&
                    ( Array.map2 (fun i i' -> i' - i) is aliveIndices)
                      |> Array.distinct
                      |> Array.length = 1)
            match previousMatch with
            | None -> loop (generation+1L) (tick rules state) ((generation, aliveIndices) :: previousAliveIndices)
            | Some (g,previousAliveIndices) ->
                let diffPerGeneration = (int64 aliveIndices.[0] - int64 previousAliveIndices.[0]) / (generation - g)
                let generationsRemaining = generations - generation
                let shift = diffPerGeneration * generationsRemaining
                let finalAliveIndices = Array.map (fun i -> int64 i + shift) aliveIndices
                finalAliveIndices

    let padded = Array.concat [ Array.replicate padding false; initialState; Array.replicate padding false ]
    loop 0L padded []

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

let part2 () =
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
    aliveIndicesAfter 50000000000L initialState ruleBook
    |> Array.sum