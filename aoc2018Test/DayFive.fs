module DayFive

open Xunit
open DayFive

[<Fact>]
let ``reducePolymer works`` () =
    let input = "dabAcCaCBAcCcaDA"
    let expected = 10
    let actual = reducePolymer input
    Assert.Equal(expected , actual)