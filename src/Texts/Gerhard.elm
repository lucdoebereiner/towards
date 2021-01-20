module Texts.Gerhard exposing (texts)

import Texts


texts : List Texts.EntryWithIndex
texts =
    [ ( 0, Texts.noNl "test1 gerhard" )
    , ( 9, Texts.noNl "test2" )
    ]



{-
   texts : List Texts.Entry
   texts =
       --  "01                                      02                                      03                                      04                                      05                                      06                                      07                                      08                                      09                                      10                                      11                                      12                                      13                                      14                                      15                                      16                                      17                                      18                                      19                                      20                                      21                                      22                                      23                                      24                                      25                                      26                                      27                                      28                                      29                                      ",
       [ "Artistic research is a radical form of aesthetic practice. It critically engages with the roots of the artist’s doing and thinking.                                                                                                                                                                                                                                                                                               Artistic research does not rival any other form of research but will use whatever existing research paradigms adapted or adaptable to the aesthetic practice at hand.                                                                                                                                                                                                                                                                                 Artistic research critically engages with research paradigms, which is its contribution to the general discourse and conceptual development of research                                                                                                                                                        ."
       , "                                        Artistic research is a radical form of aesthetic practice. It critically engages with the roots of the artist’s doing and thinking.                                                                                                                                                                                                                                                                                               Artistic research does not rival any other form of research but will use whatever existing research paradigms adapted or adaptable to the aesthetic practice at hand.                                                                                                                                                                                                                                                                                 Artistic research critically engages with research paradigms, which is its contribution to the general discourse and conceptual development of research.                                                                                                                "
       ]
           |> List.map Texts.noNl
-}
