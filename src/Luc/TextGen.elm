module Luc.TextGen exposing (Probabilities, entries, generateEntries, rule1)

import Array exposing (Array)
import Char
import Dict
import List.Extra as L
import Maybe.Extra as M
import Random
import Set
import Texts exposing (Entry)
import Utils exposing (rotate)


type alias Dimensions =
    { width : Int, height : Int }


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


entryFromArray : StringArray -> Entry
entryFromArray =
    Texts.noNl << stringFromArray


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

        rightNeighbor =
            case modBy 40 c of
                39 ->
                    Texts.lineOfCharN c right
                        |> Maybe.map (not << String.startsWith " ")

                _ ->
                    Nothing

        topNeighbor =
            fromPrev (c - 40)

        bottomNeighbor =
            fromPrev (c + 40)

        leftDist =
            modBy 40 c

        rightDist =
            39 - modBy 40 c
    in
    List.map (Maybe.withDefault False)
        [ leftNeighbor
        , rightNeighbor
        , topNeighbor
        , bottomNeighbor
        , if leftDist >= 1 then
            fromPrev (c - 1)

          else
            Nothing
        , if rightDist >= 1 then
            fromPrev (c + 1)

          else
            Nothing
        , if leftDist >= 2 then
            fromPrev (c - 2)

          else
            Nothing
        , if rightDist >= 2 then
            fromPrev (c + 2)

          else
            Nothing
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
    List.map (toRegionsEntry { width = 40, height = 30 })
        -- (\a ->
        --     entryFromArray <|
        --         Array.map
        --             (\s ->
        --                 if s.visible then
        --                     'x'
        --                 else
        --                     ' '
        --             )
        --             a
        -- )
        (nextGen initState (rotate 1 left) (rotate 1 right) seedAfterInit)



-- find regions


type alias RegionEntry a =
    { index : Int, label : a }


type alias Regions a =
    List (RegionEntry a)


addToRegions : Regions a -> RegionEntry a -> Regions a
addToRegions regions entry =
    if List.any (\e -> e.index == entry.index) regions then
        regions

    else
        entry :: regions


gatherRegions : Regions Int -> List ( Int, Regions Int )
gatherRegions regions =
    L.gatherEqualsBy .label regions
        |> List.map (\( r, collected ) -> ( r.label, r :: collected ))


type alias Span a =
    { a
        | line : Int
        , lineIndexStart : Int
        , lineIndexEnd : Int
    }


type alias SpanWithLength =
    Span { min : Int, max : Int }


lengthRange : Dimensions -> Span a -> SpanWithLength
lengthRange dim span =
    let
        minLength =
            (span.lineIndexEnd - span.lineIndexStart) + 1
    in
    { line = span.line
    , lineIndexStart = span.lineIndexStart
    , lineIndexEnd = span.lineIndexEnd
    , min = minLength
    , max = minLength + min 2 (dim.width - minLength)
    }


regionSpans : Dimensions -> Regions Int -> List (List SpanWithLength)
regionSpans dims regions =
    gatherRegions regions
        |> List.map (\r -> List.map (lengthRange dims) (spanGroups dims r))



-- lst needs to be sorted by .index


spanGroupsAux : List IndexWithLine -> Maybe (Span {}) -> List (Span {}) -> List (Span {})
spanGroupsAux lst span acc =
    case ( lst, span ) of
        ( [], Nothing ) ->
            acc

        ( [], Just curr ) ->
            acc ++ [ curr ]

        ( idx :: rest, Nothing ) ->
            spanGroupsAux rest
                (Just
                    { line = idx.line
                    , lineIndexStart = idx.indexInLine
                    , lineIndexEnd = idx.indexInLine
                    }
                )
                acc

        ( idx :: rest, Just current ) ->
            if
                (current.line == idx.line)
                    && (abs (idx.indexInLine - current.lineIndexStart)
                            <= 2
                            || abs (idx.indexInLine - current.lineIndexEnd)
                            <= 2
                       )
            then
                if idx.indexInLine < current.lineIndexStart then
                    spanGroupsAux rest (Just { current | lineIndexStart = idx.indexInLine }) acc

                else
                    spanGroupsAux rest (Just { current | lineIndexEnd = idx.indexInLine }) acc

            else
                spanGroupsAux rest
                    (Just
                        { line = idx.line
                        , lineIndexStart = idx.indexInLine
                        , lineIndexEnd = idx.indexInLine
                        }
                    )
                    (acc ++ [ current ])


type alias IndexWithLine =
    { line : Int
    , indexInLine : Int
    , index : Int
    }



-- assumes all regions have the same label


spanGroups : Dimensions -> ( Int, Regions Int ) -> List (Span {})
spanGroups dims ( label, regions ) =
    List.map
        (\r ->
            let
                line =
                    r.index // dims.width
            in
            { line = line, indexInLine = r.index - (line * dims.width), index = r.index }
        )
        regions
        |> List.sortBy .index
        |> (\lst -> spanGroupsAux lst Nothing [])


symbolizeRegions : Regions Int -> Regions Char
symbolizeRegions r =
    let
        dict =
            List.map .label r
                |> Set.fromList
                |> Set.toList
                |> List.indexedMap
                    (\i l ->
                        ( l, Char.fromCode (i + 33) )
                    )
                |> Dict.fromList
    in
    List.map
        (\e ->
            Maybe.map (\newLabel -> { index = e.index, label = newLabel }) <|
                Dict.get e.label dict
        )
        r
        |> M.values


neighborIndices : Dimensions -> Int -> List Int
neighborIndices { width, height } i =
    let
        line =
            i // width

        lineOffset =
            line * width

        lineIdx =
            modBy width i

        isLeft =
            lineIdx == 0

        isRight =
            lineIdx == (width - 1)

        isTopRow =
            line == 0

        isBottomRow =
            line == (height - 1)

        unless p v =
            if not p then
                Just v

            else
                Nothing

        left =
            unless isLeft (lineIdx - 1 + lineOffset)

        right =
            unless isRight (lineIdx + 1 + lineOffset)

        top =
            unless isTopRow (lineIdx + ((line - 1) * width))

        bottom =
            unless isBottomRow (lineIdx + ((line + 1) * width))

        ltCorner =
            unless (isTopRow || isLeft) (lineIdx - 1 + ((line - 1) * width))

        rtCorner =
            unless (isTopRow || isRight) (lineIdx + 1 + ((line - 1) * width))

        lbCorner =
            unless (isBottomRow || isLeft) (lineIdx - 1 + ((line + 1) * width))

        rbCorner =
            unless (isBottomRow || isRight) (lineIdx + 1 + ((line + 1) * width))
    in
    M.values [ left, right, top, bottom, ltCorner, rtCorner, lbCorner, rbCorner ]


flip : (a -> b -> c) -> b -> a -> c
flip function argB argA =
    function argA argB


collectNeighbors :
    Dimensions
    -> Int
    -> Array CharState
    -> List Int
    -> List Int
    -> Regions Int
    -> Regions Int
collectNeighbors dims l a toCheck checked acc =
    case toCheck of
        i :: nextToCheck ->
            if
                Maybe.map .visible (Array.get i a)
                    |> Maybe.withDefault False
            then
                let
                    thisEntry =
                        { index = i, label = l }

                    neighborsToCheck =
                        neighborIndices dims i
                            |> List.filter (\e -> not <| List.member e checked)

                    neighborEntries =
                        neighborsToCheck
                            |> List.map
                                (\n ->
                                    if
                                        Maybe.map .visible (Array.get n a)
                                            |> Maybe.withDefault False
                                    then
                                        Just { index = n, label = l }

                                    else
                                        Nothing
                                )
                            |> M.values
                in
                collectNeighbors dims
                    l
                    a
                    (nextToCheck ++ neighborsToCheck)
                    (i :: checked)
                    (thisEntry :: acc ++ neighborEntries)

            else
                collectNeighbors dims
                    l
                    a
                    nextToCheck
                    (i :: checked)
                    acc

        [] ->
            acc


regionsOfArray : Dimensions -> Array CharState -> Regions Int
regionsOfArray dims a =
    Array.indexedMap
        (\i s ->
            if s.visible then
                collectNeighbors dims i a [ i ] [] []

            else
                []
        )
        a
        |> Array.foldl (++) []
        |> List.foldl (flip addToRegions) []


toRegionsEntry : Dimensions -> Array CharState -> Entry
toRegionsEntry dims a =
    regionsOfArray dims a
        |> (\v ->
                let
                    _ =
                        Debug.log "regions" (regionSpans dims v)
                in
                v
           )
        |> symbolizeRegions
        |> List.foldl (\e -> Array.set e.index e.label) emptyArray
        |> entryFromArray



-- check texts and spans


checkStartWithSpan : List String -> SpanWithLength -> List String -> Maybe (List String)
checkStartWithSpan words span acc =
    let
        c =
            String.join " " acc |> String.length
    in
    if c > span.max then
        Nothing

    else if c >= span.min then
        Just acc

    else
        case words of
            [] ->
                Nothing

            first :: rest ->
                checkStartWithSpan rest span (acc ++ [ first ])


checkStartWithSpanList : List String -> List SpanWithLength -> List String -> Maybe (List String)
checkStartWithSpanList words span acc =
    case span of
        [] ->
            Just acc

        thisSpan :: otherSpans ->
            let
                thisCheck =
                    checkStartWithSpan words thisSpan []
            in
            case thisCheck of
                Nothing ->
                    Nothing

                Just lst ->
                    checkStartWithSpanList (List.drop (List.length lst) words)
                        otherSpans
                        (acc ++ lst)


correlateSpans : List String -> List SpanWithLength -> Maybe (List String)
correlateSpans words spans =
    case words of
        [] ->
            Nothing

        wordsLst ->
            case checkStartWithSpanList wordsLst spans [] of
                Just found ->
                    Just found

                Nothing ->
                    correlateSpans (List.drop 1 words) spans
