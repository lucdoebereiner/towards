module Main exposing (main)

import Element exposing (..)
import Element.Font as Font
import Html exposing (pre)
import Html.Attributes as Attributes
import List.Extra as L
import Texts.David as David
import Texts.Gerhard as Gerhard
import Texts.Luc as Luc
import Texts.Ludvig as Ludvig


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


format : String -> String
format s =
    s
        |> String.padRight (40 * 30) ' '
        |> insertNewlinesEveryN 40


textColumn : String -> Element msg
textColumn t =
    el [ width shrink, htmlAttribute (Attributes.style "line-height" "2") ] <| html (pre [] [ Html.text t ])


iteration : String -> String -> String -> String -> Element msg
iteration david gerhard luc ludvig =
    row [ centerX, centerY ]
        [ textColumn (format david)
        , textColumn " "
        , textColumn (format gerhard)
        , textColumn " "
        , textColumn (format luc)
        , textColumn " "
        , textColumn (format ludvig)
        ]


repeatLastN : Int -> List String -> List String
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
                lst ++ List.repeat n " "

    else
        lst


padLists : List (List String) -> List (List String)
padLists lsts =
    List.map List.length lsts
        |> List.maximum
        |> Maybe.map (\n -> List.map (repeatLastN n) lsts)
        |> Maybe.withDefault lsts


main =
    layout [ width fill, centerY, padding 20, Font.size 14, Font.family [ Font.monospace ] ] <|
        column [ width fill, spacingXY 0 30 ] <|
            List.map
                (\l ->
                    case l of
                        [ d, g, luc, ludvig ] ->
                            iteration d g luc ludvig

                        _ ->
                            let
                                _ =
                                    Debug.log "l" l
                            in
                            text "Problem"
                )
                (padLists
                    [ David.texts
                    , Gerhard.texts
                    , Luc.texts
                    , Ludvig.texts
                    ]
                    |> L.transpose
                )
