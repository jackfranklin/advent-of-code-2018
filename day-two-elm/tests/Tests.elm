module Tests exposing (suite)

import Dict
import Expect exposing (Expectation)
import Main
import Test exposing (..)


partOneTestInput =
    """
abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab
"""


suite : Test
suite =
    describe "Day Two"
        [ test "countFrequenciesOfLetters" <|
            \() ->
                Main.countFrequenciesOfLetters "bababc"
                    |> Expect.equal
                        (Dict.fromList
                            [ ( 'b', 3 )
                            , ( 'a', 2 )
                            , ( 'c', 1 )
                            ]
                        )
        , test "it calculates the checksum correctly" <|
            \() ->
                Main.calculateChecksum partOneTestInput
                    |> Expect.equal 12
        ]
