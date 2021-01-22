module PageIndices exposing
    ( Author(..)
    , PageIndices
    , authorIndex
    , authorsInOrder
    , default
    , fromTuples
    , fromUrl
    , getIndex
    , incIndex
    , indicesParser
    , nextIndex
    , previousIndex
    , setIndex
    , toUrl
    )

import Basics.Extra exposing (fractionalModBy)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias PageIndices =
    { le : Float
    , dp : Float
    , ge : Float
    , ld : Float
    , rotation : Int
    }


type Author
    = Luc
    | Gerhard
    | David
    | Ludvig


rotate : Int -> List a -> List a
rotate n l =
    let
        modN =
            modBy (List.length l) n
    in
    List.drop modN l ++ List.take modN l


columnOrder =
    [ David, Gerhard, Luc, Ludvig ]


authorIndex author =
    case author of
        David ->
            0

        Gerhard ->
            1

        Luc ->
            2

        Ludvig ->
            3


authorsInOrder : PageIndices -> List Author
authorsInOrder i =
    rotate i.rotation columnOrder


default =
    PageIndices 0.0 0.0 0.0 0.0 0


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


nextIndex : Author -> Int -> PageIndices -> PageIndices
nextIndex author max indices =
    let
        newIdx =
            fractionalModBy (toFloat max)
                (toFloat <| floor (getIndex author indices) + 1)
                |> roundFloat
    in
    setIndex author newIdx indices


previousIndex : Author -> Int -> PageIndices -> PageIndices
previousIndex author max indices =
    let
        newIdx =
            fractionalModBy (toFloat max)
                (toFloat <| ceiling (getIndex author indices) - 1)
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


indicesParser =
    Url.Parser.top
        <?> parseFloat "ludvig"
        <?> parseFloat "david"
        <?> parseFloat "gerhard"
        <?> parseFloat "luc"
        <?> (Query.map (Maybe.withDefault 0) <| Query.int "rotation")


parsePageIndices : Parser (PageIndices -> a) a
parsePageIndices =
    Url.Parser.map PageIndices indicesParser


toUrl : PageIndices -> String
toUrl p =
    Builder.absolute []
        [ Builder.string "david" (String.fromFloat p.dp)
        , Builder.string "luc" (String.fromFloat p.ld)
        , Builder.string "gerhard" (String.fromFloat p.ge)
        , Builder.string "ludvig" (String.fromFloat p.le)
        , Builder.string "rotation" (String.fromInt p.rotation)
        ]


fromUrl : Url -> PageIndices
fromUrl url =
    Maybe.withDefault
        default
        (Url.Parser.parse parsePageIndices url)
