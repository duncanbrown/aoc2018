module DayFifteen

open Xunit
open DayFifteen
open System

[<Theory>]
[<InlineData(27730, @"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")>]
[<InlineData(36334, @"#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######")>]
[<InlineData(18740, @"#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########")>]
let ``run works`` expected (input: string) =
    let actual = run (parseWorld (input.Split(Environment.NewLine)))
    Assert.Equal(expected, actual)