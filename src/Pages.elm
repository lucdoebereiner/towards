module Pages exposing (fromUrl, indices, root, toUrl, withAudio)

import Dict
import PageIndices exposing (PageIndices)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type Page
    = Root PageIndices Bool


root =
    Root


indices : Page -> PageIndices
indices (Root i _) =
    i


withAudio : Page -> Bool
withAudio (Root _ a) =
    a


audio : Query.Parser Bool
audio =
    Query.enum "audio" (Dict.fromList [ ( "true", True ), ( "false", False ) ])
        |> Query.map (Maybe.withDefault True)


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault (Root PageIndices.default True)
        (Url.Parser.parse
            (Url.Parser.map (\l d g ld r a -> Root (PageIndices l d g ld r) a)
                (PageIndices.indicesParser <?> audio)
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
            ++ [ Builder.string "audio" (boolToString (withAudio p)) ]
        )
