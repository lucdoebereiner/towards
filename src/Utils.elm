module Utils exposing (rotate)


rotate : Int -> List a -> List a
rotate n l =
    let
        modN =
            modBy (List.length l) n
    in
    List.drop modN l ++ List.take modN l
