module DaySixteen

open Xunit
open DaySixteen
open System

[<Fact>]
let ``behavesLikeOpCodes works`` () =
    let input = [|3;2;1;1|], [|9;2;1;2|], [|3;2;2;1|]
    let actual = behavesLikeOpCodes input
    Assert.Equal(3, actual)