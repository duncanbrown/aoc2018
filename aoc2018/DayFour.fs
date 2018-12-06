module DayFour

open System
open FParsec
open System.IO


type Log =
    BeginShift of int * DateTime
    | FallAsleep of DateTime
    | WakeUp of DateTime
    | DummyEnd

type ProcessedLog = (int * DateTime * int * bool)
type ProcessedLogs = ProcessedLog list

// [1518-11-01 00:00] Guard #10 begins shift
//  falls asleep
//  wakes up
type InputParser = FParsec.Primitives.Parser<Log list, unit>
let parser: InputParser =
    let parseLog =
        pchar '['
        >>. anyString 16 |>> DateTime.Parse
        .>> pchar ']'
        .>> spaces
        >>= (fun date ->
            choice [
                pstring "Guard #" >>. pint32 .>> pstring " begins shift" |>> fun id -> BeginShift (id, date);
                pstring "falls asleep" >>% FallAsleep date;
                pstring "wakes up" >>% WakeUp date
            ])
    many (parseLog .>> spaces)



let mostAsleep logs =
    let sorted = 
        logs
        |> List.sortBy (function
             | BeginShift (_, d) -> d
             | FallAsleep d -> d
             | WakeUp d -> d)
    let folder (processed: ProcessedLogs, currentId, (currentlyAwake: bool), currentDate, inProgress: ProcessedLogs) = function
        | BeginShift (id, d) ->
            let newDate = if d.Hour = 0 then d.Date else d.Date.AddDays(1.0)
            let newProcessed =
                if inProgress = [] then processed
                else
                    let (_,_,lastMinute,_) = List.last inProgress
                    let minutesToAdd =
                        [lastMinute+1 .. 59] |> List.map (fun m -> (currentId, currentDate, m, currentlyAwake))
                    let newInProgress = List.append inProgress minutesToAdd
                    List.append processed newInProgress
            (newProcessed, id, true, newDate, [])
        | FallAsleep d ->
            let (_,_,lastMinute,_) = List.tryLast inProgress |> Option.defaultValue (0,DateTime.MinValue,-1,false)
            let minutesToAdd =
                [lastMinute+1 .. d.Minute-1] |> List.map (fun m -> (currentId, currentDate, m, true))
            let newInProgress = List.append inProgress minutesToAdd
            (processed, currentId, false, currentDate, newInProgress)
        | WakeUp d ->
            let (_,_,lastMinute,_) = List.tryLast inProgress |> Option.defaultValue (0,DateTime.MinValue,-1,false)
            let minutesToAdd =
                [lastMinute+1 .. d.Minute-1] |> List.map (fun m -> (currentId, currentDate, m, false))
            let newInProgress = List.append inProgress minutesToAdd
            (processed, currentId, true, currentDate, newInProgress)
        | DummyEnd ->
            let newProcessed = List.append processed inProgress
            (newProcessed, currentId, true, currentDate, [])
        
    let processedLogs: ProcessedLogs =
        let (logs, _, _, _, _) =
            List.append sorted [DummyEnd]
            |> List.fold folder ([], 0, false, DateTime.MinValue, [])
        logs        

    let logsById = List.groupBy (fun (id, _, _, _) -> id) processedLogs

    let asleepOnly = List.filter (fun (_,_,_,awake) -> not awake)

    let minuteMostAsleep logs =
        logs
        |> asleepOnly
        |> List.countBy (fun (_,_,m,_) -> m)
        |> List.maxBy snd

    let mostAsleepId =
        logsById
        |> List.maxBy (fun (_, records) ->
            records
            |> minuteMostAsleep
            |> snd)
        |> fst

    let logsForMostAsleep =
        List.find (fun (id,_) -> id = mostAsleepId) logsById
        |> snd

    let minutesMostAsleep =
        logsForMostAsleep
        |> minuteMostAsleep
        |> fst

    mostAsleepId * minutesMostAsleep
        
    
let part2 () =
    let input = File.ReadAllText "c:\\dev\\aoc2018\\input\\4.txt"
    let parserResult = run parser input
    match parserResult with
    | Success (logs, _, _) -> mostAsleep logs
    | Failure (err,_, _) -> raise(new Exception(err))
