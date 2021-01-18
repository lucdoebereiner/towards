module PageIndices exposing
    ( Author(..)
    , PageIndices
    , default
    , fromTuples
    , getIndex
    , incIndex
    , setIndex
    )

import Basics.Extra exposing (fractionalModBy)


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


incIndex : Author -> Float -> Int -> PageIndices -> PageIndices
incIndex author inc max indices =
    let
        newIdx =
            fractionalModBy (toFloat max) (inc + getIndex author indices)
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
