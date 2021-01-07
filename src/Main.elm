module Main exposing (main)

import Element exposing (..)
import Element.Font as Font
import Html exposing (pre)


gerhard1 =
    """

Artistic research is a radical
form of aesthetic practice. It
critically engages with the
roots of the artist’s doing
and thinking.









Artistic research does not
rival any other form of
research but will use whatever
existing research paradigms
adapted or adaptable to the
aesthetic practice at hand.









Artistic research critically
engages with research para-
digms, which is its contri-
bution to the general dis-
course and conceptual develop-
development of research."""


insertNewlinesEveryN : Int -> String -> String
insertNewlinesEveryN n s =
    case s of
        "" ->
            ""

        str ->
            String.left n str ++ "\n" ++ insertNewlinesEveryN n (String.dropLeft n str)


lucStr1 =
    "Artistic practice has a conceptual and an aesthetic dimension. Their irreconcilability is a prerequisite.                                                                                                                     Aesthetic thought demonstrates that thought is not tied to language, but that there are unconscious, bodily, felt, material, and practiced forms of thought. Aesthetic thought does not reflect its object at a distance but it is a speculative action that transforms and interacts with material.                                                                                                                                                                                                         There is no method. This is not because artistic practice is based on turmoil, inspiration or whim, but because no abstract scheme of operation can be subtracted from its material entanglement."
        |> String.padRight (40 * 30) ' '
        |> insertNewlinesEveryN 30


davidStr1 =
    "                                                                                          Artistic research is in its   essence a collective and      collaborative endeavour.                                                                  Artistic research is situated,particular and subjective. It is neither valid nor          objective.                                                                                                              Artistic research does not    produce nor transport         knowledge.                                                                                                                                            Artistic research             stages the conditions for     aesthetic thinking.                                                                                                                                                                                               Formal and material practices are interwoven to each other, nevertheless maintain an      incompressible difference.    This difference is where the  generative potential for      aesthetic research lies.      Artistic research explores andexposes this gap.             "
        |> String.padRight (40 * 30) ' '
        |> insertNewlinesEveryN 30


ludvig1 =
    """

Artistic research is a more
narrow term than art.





Artistic research encompasses
a subset of artistic
activities.





Artistic research does not
require aesthetic adaptation,
but certain aesthetic
approaches are more compatible
with the superficial traits
of traditional modes of
research.





The idea of artistic research
is a child of its time, it
could not have existed 50
years ago, and it might not
exist 50 years from now."""


sampleText =
    """aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
cccccccccccccccccccccccccccccc
dddddddddddddddddddddddddddddd"""


textColumn : String -> Element msg
textColumn t =
    el [ width shrink ] <| html (pre [] [ Html.text t ])


main =
    layout [ width fill, centerY, Font.size 14 ] <|
        row [ spacingXY 20 0, centerX, centerY ]
            [ textColumn gerhard1
            , textColumn davidStr1
            , textColumn lucStr1
            , textColumn ludvig1
            ]
