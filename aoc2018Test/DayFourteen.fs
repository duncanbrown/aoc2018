module DayFourteen

open Xunit
open DayFourteen

[<Theory>]
[<InlineData(9, "5158916779")>]
[<InlineData(5, "0124515891")>]
[<InlineData(18, "9251071085")>]
[<InlineData(2018, "5941429882")>]
let ``getNext10 works`` after (expected: string) =
    let actual = getNext10 after
    Assert.Equal(expected |> Seq.map (string >> int) |> List.ofSeq, actual)