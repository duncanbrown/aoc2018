module DayFour

open System
open Xunit
open DayFour
open FParsec

[<Fact>]
let ``Input parser works`` () =
    let input = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up"
    let expected = [
        BeginShift (10, DateTime.Parse("1518-11-01 00:00"));
        FallAsleep (DateTime.Parse("1518-11-01 00:05"));
        WakeUp (DateTime.Parse("1518-11-01 00:25"));
        ]
    let actual = run parser input
    match actual with
    | Success(result, _, _) ->  Assert.Equal<Log list>(expected, result)
    | Failure(err, _, _) -> Assert.False(true, err)


[<Fact>]
let ``mostAsleep works`` () =
    let input = [
        BeginShift (10, DateTime.Parse("1518-11-01 00:00"));
        FallAsleep (DateTime.Parse("1518-11-01 00:05"));
        WakeUp (DateTime.Parse("1518-11-01 00:25"));
        FallAsleep (DateTime.Parse("1518-11-01 00:30"));
        WakeUp (DateTime.Parse("1518-11-01 00:55"));
        BeginShift (99, DateTime.Parse("1518-11-01 23:58"));
        FallAsleep (DateTime.Parse("1518-11-02 00:40"));
        WakeUp (DateTime.Parse("1518-11-02 00:50"));
        BeginShift (10, DateTime.Parse("1518-11-03 00:05"));
        FallAsleep (DateTime.Parse("1518-11-03 00:24"));
        WakeUp (DateTime.Parse("1518-11-03 00:29"));
        BeginShift (99, DateTime.Parse("1518-11-04 00:02"));
        FallAsleep (DateTime.Parse("1518-11-04 00:36"));
        WakeUp (DateTime.Parse("1518-11-04 00:46"));
        BeginShift (99, DateTime.Parse("1518-11-05 00:03"));
        FallAsleep (DateTime.Parse("1518-11-05 00:45"));
        WakeUp (DateTime.Parse("1518-11-05 00:55"));

    ]
    let expected = 4455
    let actual = mostAsleep input
    Assert.Equal(expected, actual)