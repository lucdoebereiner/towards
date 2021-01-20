module Texts.Ludvig exposing (texts)

import Texts


texts : List Texts.EntryWithIndex
texts =
    [ ( 0, Texts.noNl "test1" )
    , ( 9, Texts.nlClip """test2
another line

and another one""" )
    ]



{-
   texts : List Texts.Entry
   texts =
       [ "Artistic research is a more narrow term than art.                                                                                                                                                           Artistic research encompasses a subset of artistic activities.                                                                                                                                                                                      Artistic research does not      require aesthetic adaptation,           but certain aesthetic approaches are    more compatible with the superficial traits of traditional modes of research.                                                                                                                                                                   The idea of Artistic Research is a childof its time, it could not have existed  50 years ago, and it might not exist 50 years from now."
       , "                                                               Artistic Research evokes directionality,                              spatial relationships with art. Such as                   through                    towards                      in                 about                      .                                                                                                                                  Perhaps thinkingin terms of directions and becomings    is more generative than trying to pin-  point a position on the map where       artistic research is supposed to be     located.                                                                                                                                                                                                                                                                                                                                           "
       ]
           |> List.map Texts.noNl
-}
