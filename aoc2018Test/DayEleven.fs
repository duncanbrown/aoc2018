module DayEleven

open Xunit
open DayEleven

[<Theory>]
[<InlineData(3,5,8,4)>]
[<InlineData(122,79,57,-5)>]
[<InlineData(217,196,39,0)>]
[<InlineData(101,153,71,4)>]
let ``getPowerLevel works`` x y serialNumber expected =
    let actual = getPowerLevel serialNumber (x,y)
    Assert.Equal(expected, actual)

[<Fact>]
let ``getLargestTotalPower works`` () =
    let actual = getLargestTotalPower 18
    Assert.Equal((33,45), actual)

[<Fact>]
let ``getLargestTotalPowerOfAnySize works`` () =
    let actual = getLargestTotalPowerOfAnySize 18
    Assert.Equal((16, (90,269)), actual)