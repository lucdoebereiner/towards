module Pages exposing (fromUrl, indices, withAudio)

import Dict
import PageIndices exposing (..)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type Page
    = Root PageIndices Bool


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
    Maybe.withDefault (Root default True)
        (Url.Parser.parse
            (Url.Parser.map (\l d g ld r a -> Root (PageIndices l d g ld r) a)
                (indicesParser <?> audio)
            )
            url
        )
