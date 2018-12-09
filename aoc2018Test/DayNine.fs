module DayNine

open Xunit
open DayNine

[<Theory>]
[<InlineData(5, 25, 32)>]
[<InlineData(10, 1618, 8317)>]
[<InlineData(13, 7999, 146373)>]
[<InlineData(17, 1104, 2764)>]
[<InlineData(21, 6111, 54718)>]
[<InlineData(30, 5807, 37305)>]
let ``score works`` players lastMarble expected =
    let actual = score players lastMarble
    Assert.Equal(expected , actual)