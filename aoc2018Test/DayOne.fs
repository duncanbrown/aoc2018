module Tests

open System
open Xunit
open DayOne
open FParsec

[<Fact>]
let ``Input parser works`` () =
    let input = "+10\n-2 \n+56666"
    let expected: Program = [(Plus, 10L);(Minus, 2L);(Plus, 56666L)]
    let actual = run inputParser input
    match actual with
    | Success(result, _, _) ->  Assert.Equal<Program>(expected, result)
    | Failure(err, _, _) -> Assert.False(true, err)

[<Fact>]
let ``Calculator works`` () =
    let program: Program = [(Plus, 10L);(Minus, 2L);(Plus, 4L)]
    let expected = 12L
    let actual = calculator 0L program
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData("+1-1", 0)>]
[<InlineData("+7+7-2-7-4", 14)>]
[<InlineData("+3+3+4-2-4", 10)>]
[<InlineData("-6+3+8+5-6", 5)>]
let ``Repeat detector works`` input expected =
    match run inputParser input with
    | Success(program, _, _) ->
        let actual = repeatDetector program
        Assert.Equal(expected, actual)
    | Failure(err, _, _) -> Assert.False(true, err)
