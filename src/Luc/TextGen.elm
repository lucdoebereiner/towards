module Luc.TextGen exposing (entries, rule1)

import Array exposing (Array)
import Maybe.Extra as M
import Texts exposing (Entry)


type alias StringArray =
    Array Char


arrayFromString : String -> StringArray
arrayFromString s =
    String.toList s |> Array.fromList


stringFromArray : StringArray -> String
stringFromArray a =
    Array.toList a |> String.fromList


arrayFromEntry : Entry -> StringArray
arrayFromEntry e =
    Texts.entryString e |> String.replace "\n" "" |> arrayFromString


emptyArray : StringArray
emptyArray =
    Array.repeat (40 * 30) ' '


get : Int -> StringArray -> Maybe Char
get n array =
    Array.get n array


isInWordContext : Int -> StringArray -> Bool
isInWordContext i array =
    M.values
        [ get (max (i - 1) 0) array
        , get i array
        , get (i + 1) array
        ]
        |> List.any (\c -> not (c == ' '))


stepChar :
    Int
    -> (Bool -> Bool -> Bool -> Bool -> Bool)
    -> Entry
    -> Entry
    -> StringArray
    -> StringArray
    -> Bool
stepChar i rule left right prev current =
    let
        lL =
            Texts.lineOfCharN i left
                |> Maybe.map (not << String.endsWith " ")
                |> Maybe.withDefault False

        lR =
            Texts.lineOfCharN i right
                |> Maybe.map (not << String.startsWith " ")
                |> Maybe.withDefault False

        pi =
            isInWordContext i prev

        pc =
            isInWordContext (min (i - 1) 0) current
    in
    rule pi lL lR pc


stepEntry :
    (Bool -> Bool -> Bool -> Bool -> Bool)
    -> Entry
    -> Entry
    -> StringArray
    -> Entry
stepEntry rule left right prev =
    let
        stepEntryAux i current =
            if i >= Array.length current then
                current

            else
                let
                    newCurent =
                        if stepChar i rule left right prev current then
                            Array.set i 'x' current

                        else
                            current
                in
                stepEntryAux (i + 1) newCurent
    in
    stepEntryAux 0 (Array.repeat (40 * 30) ' ')
        |> stringFromArray
        |> Texts.noNl


entries :
    (Bool -> Bool -> Bool -> Bool -> Bool)
    -> List Entry
    -> List Entry
    -> List Entry
entries rule left right =
    let
        entriesAux l r c =
            case ( l, r ) of
                ( [], [] ) ->
                    []

                ( l1 :: lrest, r1 :: rrest ) ->
                    let
                        nextEntry =
                            stepEntry rule l1 r1 (arrayFromEntry c)
                    in
                    nextEntry :: entriesAux lrest rrest nextEntry

                _ ->
                    []
    in
    entriesAux left right ((Texts.noNl << stringFromArray) emptyArray)


rule1 : Bool -> Bool -> Bool -> Bool -> Bool
rule1 prevIt lL lR prevChar =
    case [ prevIt, lL, lR, prevChar ] of
        [ False, False, False, False ] ->
            True

        [ False, False, False, True ] ->
            True

        [ False, False, True, False ] ->
            False

        [ False, False, True, True ] ->
            False

        [ False, True, False, False ] ->
            True

        [ False, True, False, True ] ->
            False

        [ False, True, True, False ] ->
            False

        [ False, True, True, True ] ->
            True

        [ True, False, False, False ] ->
            False

        [ True, False, False, True ] ->
            True

        [ True, False, True, False ] ->
            True

        [ True, False, True, True ] ->
            True

        [ True, True, False, False ] ->
            True

        [ True, True, False, True ] ->
            True

        [ True, True, True, False ] ->
            True

        [ True, True, True, True ] ->
            False

        _ ->
            False
