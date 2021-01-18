module Texts exposing (Texts, getText, indexTexts, length, setAuthorTextAt)

import List.Extra as L
import PageIndices exposing (Author(..), PageIndices)


type alias Texts =
    { ge : List String, dp : List String, ld : List String, le : List String }


maxLength : List (List a) -> Int
maxLength lsts =
    List.map List.length lsts
        |> List.maximum
        |> Maybe.withDefault 0


indexTexts : Author -> PageIndices -> Texts -> String
indexTexts author indices texts =
    L.getAt
        (round (PageIndices.getIndex author indices))
        (getText author texts)
        |> Maybe.withDefault " "


length : Texts -> Int
length p =
    [ p.ge, p.dp, p.ld, p.le ]
        |> maxLength


getText : Author -> Texts -> List String
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


setText : Author -> List String -> Texts -> Texts
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


setAuthorTextAt : Author -> PageIndices -> String -> Texts -> Texts
setAuthorTextAt author indices str texts =
    let
        newLst =
            L.setAt (round (PageIndices.getIndex author indices)) str (getText author texts)
    in
    setText author newLst texts
