module DaySix

open Xunit
open DaySix

[<Fact>]
let ``sizeOfLargestArea works`` () =
    let input = [1,1;1,6;8,3;3,4;5,5;8,9]
    let expected = 17
    let actual = sizeOfLargestArea input
    Assert.Equal(expected , actual)

[<Fact>]
let ``sizeORegion works`` () =
    let input = [1,1;1,6;8,3;3,4;5,5;8,9]
    let expected = 16
    let actual = sizeOfRegion 32 input
    Assert.Equal(expected , actual)