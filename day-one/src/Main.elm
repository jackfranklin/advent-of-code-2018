module Main exposing (findFirstDuplicateFrequency, part1, part2, sumFrequency)

import Input
import Set


part1 =
    sumFrequency Input.puzzleInput


part2 =
    findFirstDuplicateFrequency Input.puzzleInput


sumFrequency : String -> Int
sumFrequency input =
    input
        |> String.split "\n"
        |> List.filter (String.isEmpty >> not)
        |> List.filterMap String.toInt
        |> List.foldl
            (\currentValue acc ->
                acc + currentValue
            )
            0


findFirstDuplicateFrequency : String -> Int
findFirstDuplicateFrequency input =
    input
        |> String.split "\n"
        |> List.filter (String.isEmpty >> not)
        |> List.filterMap String.toInt
        |> stepThroughFrequencyChanges


stepThroughFrequencyChanges : List Int -> Int
stepThroughFrequencyChanges frequencies =
    step frequencies frequencies Set.empty 0


step fullFrequencies loopingFrequencies seenValues acc =
    case loopingFrequencies of
        [] ->
            step fullFrequencies fullFrequencies seenValues acc

        x :: xs ->
            let
                newAcc =
                    acc + x

                newSeenValues =
                    Set.insert newAcc seenValues
            in
            if Set.member newAcc seenValues then
                newAcc

            else
                step fullFrequencies xs newSeenValues newAcc
