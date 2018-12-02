module DayTwo

open System
open Xunit
open DayTwo

[<Fact>]
let ``checksum works`` () =
    let input = [
        "abcdef";
        "bababc";
        "abbcde";
        "abcccd";
        "aabcdd";
        "abcdee";
        "ababab"
    ]
    let expected = 12
    let actual = checksum input
    Assert.Equal(expected, actual)

[<Fact>]
let ``findCommon works`` () =
    let input = [
        "abcde";
        "fghij";
        "klmno";
        "pqrst";
        "fguij";
        "axcye";
        "wvxyz"
    ]
    let expected = "fgij"
    let actual = findCommon input
    Assert.Equal(expected, actual)