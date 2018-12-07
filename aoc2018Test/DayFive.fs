module DayFive

open Xunit
open DayFive

[<Fact>]
let ``reducePolymer works`` () =
    let input = "dabAcCaCBAcCcaDA"
    let expected = 10
    let actual = reducePolymer input
    Assert.Equal(expected , actual)

[<Fact>]
let ``pruneAndReducePolymer works`` () =
    let input = "dabAcCaCBAcCcaDA"
    let expected = 4
    let actual = pruneAndReduce input
    Assert.Equal(expected , actual)