module Texts.Intro exposing (texts)

import Element exposing (..)
import Element.Font as Font
import Element.Region as Region


texts : Element msg
texts =
    column [ width fill, spacing 20 ]
        [ el [ Region.heading 1, Font.bold, Font.size 26 ] <| text "Towards"
        , paragraph [] [ text "Luc Döbereiner, Gerhard Eckel, Ludvig Elblaus, David Pirrò" ]
        , paragraph [ paddingEach { top = 30, left = 0, right = 0, bottom = 0 } ]
            [ text "Doing and thinking..."
            ]
        ]
