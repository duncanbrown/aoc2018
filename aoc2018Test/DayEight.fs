module DayEight

open Xunit
open DayEight

[<Fact>]
let ``licenceSum works`` () =
    let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let expected = 138
    let actual = licenceSum input
    Assert.Equal(expected , actual)

[<Fact>]
let ``value works`` () =
    let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let expected = 66
    let actual = value input
    Assert.Equal(expected , actual)