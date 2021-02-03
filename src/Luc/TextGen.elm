module Luc.TextGen exposing (Probabilities, entries, generateEntries, rule1)

import Array exposing (Array)
import List.Extra as L
import Maybe.Extra as M
import Random
import Texts exposing (Entry)
import Utils exposing (rotate)


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
            False

        [ False, False, False, True ] ->
            False

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



-- "clouds" based on 10.1063/1.4866854


type alias CharState =
    { potential : Bool
    , active : Bool
    , visible : Bool
    }


nextCharState : List Bool -> CharState -> CharState
nextCharState neighborsOn c =
    { potential = c.potential && not c.active
    , active =
        not c.active
            && c.potential
            && List.any identity neighborsOn
    , visible = c.visible || c.active
    }


type alias Probabilities =
    { extProb : Float
    , potentialProb : Float
    , activeProb : Float
    }


probCharState : Random.Seed -> Probabilities -> CharState -> ( CharState, Random.Seed )
probCharState seed probs state =
    Random.map3
        (\p1 p2 p3 ->
            { potential = state.potential || p1 < probs.potentialProb
            , active = state.active || p2 < probs.activeProb
            , visible = state.visible && p3 > probs.extProb
            }
        )
        probability
        probability
        probability
        |> (\g -> Random.step g seed)


neighbors : Int -> Entry -> Entry -> Array CharState -> List Bool
neighbors c left right previous =
    let
        fromPrev i =
            Maybe.map .visible <| Array.get i previous

        leftNeighbor =
            case modBy 40 c of
                0 ->
                    Texts.lineOfCharN c left
                        |> Maybe.map (not << String.endsWith " ")

                _ ->
                    Nothing

        --fromPrev (c - )1
        rightNeighbor =
            case modBy 40 c of
                39 ->
                    Texts.lineOfCharN c right
                        |> Maybe.map (not << String.startsWith " ")

                _ ->
                    Nothing

        --fromPrev (c + 1)
        -- _ =
        --     Debug.log "neighbors left and right" ( c, leftNeighbor, rightNeighbor )
        topNeighbor =
            fromPrev (c - 40)

        bottomNeighbor =
            fromPrev (c + 40)
    in
    List.map (Maybe.withDefault False)
        [ leftNeighbor
        , rightNeighbor
        , topNeighbor
        , bottomNeighbor
        , fromPrev (c - 1)
        , fromPrev (c + 1)
        , fromPrev (c - 2)
        , fromPrev (c + 2)
        ]


probability : Random.Generator Float
probability =
    Random.float 0 1


initStates : Float -> Random.Seed -> ( Array CharState, Random.Seed )
initStates prob seed =
    let
        ( lst, newSeed ) =
            Random.step (Random.list (40 * 30) probability) seed
    in
    ( lst
        |> List.map (\p -> CharState (p < prob) False False)
        |> Array.fromList
    , newSeed
    )



-- probCharState : Random.Seed -> Probabilities -> CharState -> ( CharState, Random.Seed )
-- probCharState seed probs state =


flipTuple : ( a, b ) -> ( b, a )
flipTuple ( a, b ) =
    ( b, a )


applyProbs :
    Random.Seed
    -> Probabilities
    -> Array CharState
    -> ( Array CharState, Random.Seed )
applyProbs seed probs array =
    let
        ( newSeed, states ) =
            L.mapAccuml
                (\s c ->
                    probCharState s probs c |> flipTuple
                )
                seed
                (Array.toList array)
    in
    ( Array.fromList states, newSeed )


genEntry : Entry -> Entry -> Array CharState -> Array CharState
genEntry l r p =
    Array.indexedMap
        (\i state ->
            nextCharState
                (neighbors i l r p)
                state
        )
        p


generateEntries : Probabilities -> List Entry -> List Entry -> List Entry
generateEntries probs left right =
    let
        initSeed =
            Random.initialSeed 0

        emptyState =
            Array.repeat (40 * 30) (CharState False False False)

        ( initState, seedAfterInit ) =
            case ( List.head left, List.head right ) of
                ( Just l, Just r ) ->
                    Tuple.mapFirst (genEntry l r) <|
                        applyProbs initSeed probs emptyState

                _ ->
                    ( emptyState, initSeed )

        nextGen p l r seed =
            case ( l, r ) of
                ( thisL :: restL, thisR :: restR ) ->
                    let
                        ( withProbs, newSeed ) =
                            applyProbs seed probs p

                        nextEntry =
                            genEntry thisL thisR withProbs
                    in
                    nextEntry :: nextGen nextEntry restL restR newSeed

                _ ->
                    []
    in
    List.map
        (\a ->
            (Texts.noNl << stringFromArray) <|
                Array.map
                    (\s ->
                        if s.visible then
                            'x'

                        else
                            ' '
                    )
                    a
        )
        (nextGen initState (rotate 1 left) (rotate 1 right) seedAfterInit)



-- ideas shift generation by one to react to current neighbord and start with content not empty [done]
-- redefine neighborhood to avoid line break
