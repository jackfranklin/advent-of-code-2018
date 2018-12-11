module Tests exposing (suite)

import Expect exposing (Expectation)
import Main
import Test exposing (..)


partOneInput =
    """
+5
-11
+17
"""


partTwoInput =
    """
+1
-2
+3
+1
"""


suite : Test
suite =
    describe "Day One"
        [ test "it sums up the numbers correctly" <|
            \() ->
                Main.sumFrequency partOneInput
                    |> Expect.equal 11
        , test "it finds the duplicate frequency correctly" <|
            \() ->
                Main.findFirstDuplicateFrequency partTwoInput |> Expect.equal 2
        ]
