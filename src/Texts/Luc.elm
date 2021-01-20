module Texts.Luc exposing (texts)

import Texts


texts : List Texts.EntryWithIndex
texts =
    [ ( 0, Texts.noNl "test1 luc" )
    , ( 9, Texts.noNl "test2" )
    ]



{-
   texts : List Texts.Entry
   texts =
       [ "Artistic practice has a conceptual and an aesthetic dimension. Their irreconcilability is a prerequisite.                                                                                                                     Aesthetic thought demonstrates that thought is not tied to language, but that there are unconscious, bodily, felt, material, and practiced forms of thought. Aesthetic thought does not reflect its object at a distance but it is a speculative action that transforms and interacts with material.                                                                                                                                                                                                         There is no method. This is not because artistic practice is based on turmoil, inspiration or whim, but because no abstract scheme of operation can be subtracted from its material entanglement."
       , "The irreconcilability is a prerequisite.                                                                                                                                                     Speculative practice works at a distance. Materialist artistic practice does not consist in reconciling thought and matter, but in recognizing the productive distances between matter and thoughts themselves.                                                                                                                                                                                                          No abstract scheme of operation can be subtracted from arts material entanglement.                                                                                                                                 We work with appearances."
       ]
           |> List.map Texts.noNl
-}
