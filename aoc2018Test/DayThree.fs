module DayThree

open System
open Xunit
open DayThree
open FParsec

[<Fact>]
let ``parser works`` () =
    let input = "#1 @ 1,3: 4x4 \n#2 @ 3,1: 4x4"
    let expected = [
        { Id = "1"; X = 1; Y = 3; Width = 4; Height = 4; };
        { Id = "2"; X = 3; Y = 1; Width = 4; Height = 4; };
    ]
    let actual = run inputParser input
    match actual with
    | Success(result, _, _) ->  Assert.Equal<list<Claim>>(expected, result)
    | Failure(err, _, _) -> Assert.False(true, err)

[<Fact>]
let ``getOverlapCount works`` () =
    let input = [
        { Id = "1"; X = 1; Y = 3; Width = 4; Height = 4; };
        { Id = "2"; X = 3; Y = 1; Width = 4; Height = 4; };
        { Id = "3"; X = 5; Y = 5; Width = 2; Height = 2; }
    ]
    let expected = 4
    let actual = getOverlapCount input
    Assert.Equal(expected, actual)

[<Fact>]
let ``getWinningClaim works`` () =
    let input = [
        { Id = "1"; X = 1; Y = 3; Width = 4; Height = 4; };
        { Id = "2"; X = 3; Y = 1; Width = 4; Height = 4; };
        { Id = "3"; X = 5; Y = 5; Width = 2; Height = 2; }
    ]
    let expected = "3"
    let actual = getWinningClaim input
    Assert.Equal(expected, actual)