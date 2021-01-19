module Texts exposing
    ( Entry
    , Texts
    , entryString
    , getText
    , indexTexts
    , length
    , nlClip
    , noNl
    , printEntry
    , setAuthorTextAt
    , transposedTexts
    )

import List.Extra as L
import PageIndices exposing (Author(..), PageIndices)


type alias Texts =
    { ge : List Entry, dp : List Entry, ld : List Entry, le : List Entry }


maxLength : List (List a) -> Int
maxLength lsts =
    List.map List.length lsts
        |> List.maximum
        |> Maybe.withDefault 0


indexTexts : Author -> PageIndices -> Texts -> Maybe Entry
indexTexts author indices texts =
    L.getAt
        (round (PageIndices.getIndex author indices))
        (getText author texts)


length : Texts -> Int
length p =
    [ p.ge, p.dp, p.ld, p.le ]
        |> maxLength


getText : Author -> Texts -> List Entry
getText author texts =
    case author of
        Luc ->
            texts.ld

        Gerhard ->
            texts.ge

        David ->
            texts.dp

        Ludvig ->
            texts.le


setText : Author -> List Entry -> Texts -> Texts
setText author str texts =
    case author of
        Luc ->
            { texts | ld = str }

        Gerhard ->
            { texts | ge = str }

        David ->
            { texts | dp = str }

        Ludvig ->
            { texts | le = str }


setAuthorTextAt : Author -> PageIndices -> Entry -> Texts -> Texts
setAuthorTextAt author indices str texts =
    let
        newLst =
            L.setAt (round (PageIndices.getIndex author indices))
                str
                (getText author texts)
    in
    setText author newLst texts



-- Formating


insertNewlinesEveryN : Int -> String -> String
insertNewlinesEveryN n s =
    case s of
        "" ->
            ""

        str ->
            String.left n str
                ++ "\n"
                ++ insertNewlinesEveryN n
                    (String.dropLeft n str)


ensureLength : Int -> a -> List a -> List a
ensureLength n elem lst =
    if List.length lst < n then
        ensureLength n elem (lst ++ [ elem ])

    else if List.length lst == n then
        lst

    else
        List.take n lst


emptyLine =
    String.padRight 40 ' ' " "


formatNoNl : String -> String
formatNoNl s =
    s
        |> String.replace "\n" ""
        |> String.left (40 * 30)
        |> String.padRight (40 * 30) ' '
        |> insertNewlinesEveryN 40


formatNlClip : String -> String
formatNlClip s =
    s
        |> String.lines
        |> List.map (String.left 40 << String.padRight 40 ' ')
        |> ensureLength 30 emptyLine
        |> String.join "\n"


formatPrinting : String -> String
formatPrinting s =
    s
        |> String.replace "\n" ""
        |> String.left (40 * 30)
        |> String.padRight (40 * 30) ' '


type Entry
    = NoNl String
    | NlClip String


noNl : String -> Entry
noNl s =
    NoNl (formatNoNl s)


nlClip : String -> Entry
nlClip s =
    NlClip (formatNlClip s)


entryString : Entry -> String
entryString e =
    case e of
        NoNl s ->
            s

        NlClip s ->
            s


printEntry : Entry -> String
printEntry =
    formatPrinting << entryString


repeatLastN : Int -> List a -> List a
repeatLastN n lst =
    let
        lack =
            n - List.length lst
    in
    if lack > 0 then
        case L.last lst of
            Just l ->
                lst ++ List.repeat n l

            Nothing ->
                lst

    else
        lst



-- maxLength lsts =
--     List.map List.length lsts
--         |> List.maximum


padLists : List (List a) -> List (List a)
padLists lsts =
    maxLength lsts
        |> (\n -> List.map (repeatLastN n) lsts)


transposedTexts : Texts -> List (List Entry)
transposedTexts texts =
    padLists
        [ texts.dp
        , texts.ge
        , texts.ld
        , texts.le
        ]
        |> L.transpose
