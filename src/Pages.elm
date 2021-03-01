module Pages exposing (fromUrl, indices, root, testMode, toUrl, withAudio)

import Dict
import PageIndices exposing (PageIndices)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type Page
    = Root PageIndices Bool Bool


root =
    Root


indices : Page -> PageIndices
indices (Root i _ _) =
    i


withAudio : Page -> Bool
withAudio (Root _ a _) =
    a


testMode : Page -> Bool
testMode (Root _ _ a) =
    a


audio : Query.Parser Bool
audio =
    Query.enum "audio" (Dict.fromList [ ( "true", True ), ( "false", False ) ])
        |> Query.map (Maybe.withDefault True)


test : Query.Parser Bool
test =
    Query.enum "test" (Dict.fromList [ ( "true", True ), ( "false", False ) ])
        |> Query.map (Maybe.withDefault False)


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault (Root PageIndices.default True False)
        (Url.Parser.parse
            (Url.Parser.map (\l d g ld r a t -> Root (PageIndices l d g ld r) a t)
                (PageIndices.indicesParser <?> audio <?> test)
            )
            url
        )


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "true"

        False ->
            "false"


toUrl : Page -> String
toUrl p =
    Builder.absolute []
        (PageIndices.toUrl (indices p)
            ++ [ Builder.string "audio" (boolToString (withAudio p))
               ]
            ++ (if testMode p then
                    [ Builder.string "test" "true" ]

                else
                    []
               )
        )
