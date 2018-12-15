module DayFourteen

open Xunit
open DayFourteen

[<Theory>]
[<InlineData("51589", 9)>]
[<InlineData("01245", 5)>]
[<InlineData("92510", 18)>]
[<InlineData("59414", 2018)>]
let ``getNumberToLeftOf works`` recipe expected =
    let actual = getNumberToLeftOf recipe
    Assert.Equal(expected, actual)