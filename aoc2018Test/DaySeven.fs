module DaySeven

open Xunit
open DaySeven
open FParsec

[<Fact>]
let ``order works`` () =
    let input = [
        'C', 'A';
        'C', 'F';
        'A', 'B';
        'A', 'D';
        'B', 'E';
        'D', 'E';
        'F', 'E';
    ]
    let expected = "CABDFE"
    let actual = order input
    Assert.Equal(expected , actual)

[<Fact>]
let ``Input parser works`` () =
    let input = "Step C must be finished before step A can begin. \nStep C must be finished before step F can begin."
    let expected = [
        'C', 'A';
        'C', 'F';
    ]
    let actual = run inputParser input
    match actual with
    | Success(result, _, _) ->  Assert.Equal<(char * char) list>(expected, result)
    | Failure(err, _, _) -> Assert.False(true, err)
