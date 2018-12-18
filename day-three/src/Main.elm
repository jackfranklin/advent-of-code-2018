module Main exposing (calculateChecksum, countFrequenciesOfLetters, partOne)

import Dict exposing (Dict)
import Input


incrementOrInsertLetterCount : Char -> Dict Char Int -> Dict Char Int
incrementOrInsertLetterCount letter frequencies =
    frequencies
        |> Dict.update letter
            (\value ->
                case value of
                    Nothing ->
                        Just 1

                    Just x ->
                        Just (x + 1)
            )


countFrequenciesOfLetters : String -> Dict Char Int
countFrequenciesOfLetters input =
    input
        |> String.toList
        |> List.foldl incrementOrInsertLetterCount Dict.empty


doesInputContainLetterTwice : Dict Char Int -> Bool
doesInputContainLetterTwice frequencies =
    frequencies
        |> Dict.values
        |> List.any ((==) 2)


doesInputContainLetterThrice : Dict Char Int -> Bool
doesInputContainLetterThrice frequencies =
    frequencies
        |> Dict.values
        |> List.any ((==) 3)


type alias FrequencyMatch =
    { twice : Bool
    , thrice : Bool
    }


partOne =
    calculateChecksum Input.puzzleInput


frequenciesToMatch : Dict Char Int -> FrequencyMatch
frequenciesToMatch frequencies =
    { twice = doesInputContainLetterTwice frequencies
    , thrice = doesInputContainLetterThrice frequencies
    }


calculateChecksum : String -> Int
calculateChecksum input =
    input
        |> String.split "\n"
        |> List.filter (String.isEmpty >> not)
        |> List.map countFrequenciesOfLetters
        |> List.map frequenciesToMatch
        |> List.foldl
            (\{ twice, thrice } accumulator ->
                case ( twice, thrice ) of
                    ( True, True ) ->
                        { twice = accumulator.twice + 1, thrice = accumulator.thrice + 1 }

                    ( True, False ) ->
                        { accumulator | twice = accumulator.twice + 1 }

                    ( False, True ) ->
                        { accumulator | thrice = accumulator.thrice + 1 }

                    ( False, False ) ->
                        accumulator
            )
            { twice = 0, thrice = 0 }
        |> (\{ twice, thrice } -> twice * thrice)
