module PageIndices exposing
    ( Author(..)
    , PageIndices
    , default
    , fromTuples
    , getIndex
    , incIndex
    , parsePageIndices
    , setIndex
    , toUrl
    )

import Basics.Extra exposing (fractionalModBy)
import Url
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias PageIndices =
    { le : Float
    , dp : Float
    , ge : Float
    , ld : Float
    }


type Author
    = Luc
    | Gerhard
    | David
    | Ludvig


default =
    PageIndices 0.0 0.0 0.0 0.0


roundFloat : Float -> Float
roundFloat f =
    round (f * 1000)
        |> toFloat
        |> (\i -> i / 1000)


incIndex : Author -> Float -> Int -> PageIndices -> PageIndices
incIndex author inc max indices =
    let
        newIdx =
            fractionalModBy (toFloat max) (inc + getIndex author indices)
                |> roundFloat
    in
    setIndex author newIdx indices


getIndex : Author -> PageIndices -> Float
getIndex author indices =
    case author of
        Luc ->
            indices.ld

        Gerhard ->
            indices.ge

        David ->
            indices.dp

        Ludvig ->
            indices.le


setIndex : Author -> Float -> PageIndices -> PageIndices
setIndex author f indices =
    case author of
        Luc ->
            { indices | ld = f }

        Gerhard ->
            { indices | ge = f }

        David ->
            { indices | dp = f }

        Ludvig ->
            { indices | le = f }


fromTuples : List ( Author, Float ) -> PageIndices
fromTuples pairs =
    List.foldl (\( author, index ) indices -> setIndex author index indices) default pairs



-- url parsing


parseFloat : String -> Query.Parser Float
parseFloat key =
    Query.map (Maybe.withDefault 0.0) <|
        Query.custom
            key
        <|
            \stringList ->
                case stringList of
                    [ str ] ->
                        String.toFloat str

                    _ ->
                        Nothing


parsePageIndices : Parser (PageIndices -> a) a
parsePageIndices =
    Url.Parser.map PageIndices
        (Url.Parser.top
            <?> parseFloat "ludvig"
            <?> parseFloat "david"
            <?> parseFloat "gerhard"
            <?> parseFloat "luc"
        )


toUrl : PageIndices -> String
toUrl p =
    Builder.absolute []
        [ Builder.string "david" (String.fromFloat p.dp)
        , Builder.string "luc" (String.fromFloat p.ld)
        , Builder.string "gerhard" (String.fromFloat p.ge)
        , Builder.string "ludvig" (String.fromFloat p.le)
        ]
