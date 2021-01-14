module Main exposing (main)

import Animator
import Animator.Css
import Animator.Inline
import Browser
import Browser.Events
import Browser.Navigation
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (pre)
import Html.Attributes as Attributes
import List.Extra as L
import Texts.David as David
import Texts.Gerhard as Gerhard
import Texts.Luc as Luc
import Texts.Ludvig as Ludvig
import Time
import Url


type alias Model =
    { pageIndices : Animator.Timeline PageIndices
    , navKey : Browser.Navigation.Key
    , needsUpdate : Bool
    }


type alias PageIndices =
    { le : Int
    , dp : Int
    , ge : Int
    , ld : Int
    }


main =
    Browser.application
        { init =
            \() url navKey ->
                ( { pageIndices = Animator.init (PageIndices 0 0 0 0)
                  , navKey = navKey
                  , needsUpdate = False
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ animator
                        |> Animator.toSubscription Tick model
                    ]
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }


type Msg
    = Tick Time.Posix
    | ClickedLink Browser.UrlRequest
    | UrlChanged Url.Url
    | SetPage PageIndices


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )

        ClickedLink _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        SetPage newIndices ->
            ( { model
                | pageIndices =
                    model.pageIndices
                        |> Animator.go (Animator.seconds 5) newIndices
              }
            , Cmd.none
            )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching .pageIndices
            (\newIndices model ->
                { model | pageIndices = newIndices }
            )


insertNewlinesEveryN : Int -> String -> String
insertNewlinesEveryN n s =
    case s of
        "" ->
            ""

        str ->
            String.left n str
                ++ "\n"
                ++ insertNewlinesEveryN n
                    (String.dropLeft n str)


format : String -> String
format s =
    s
        |> String.padRight (40 * 30) ' '
        |> insertNewlinesEveryN 40



--Attributes.style "opacity" "0.1" ]


textColumn :
    Animator.Timeline PageIndices
    -> (PageIndices -> Int)
    -> Int
    -> String
    -> Element msg
textColumn timeline accessCurrent index t =
    el
        [ width shrink
        , htmlAttribute (Attributes.style "line-height" "2")
        ]
    <|
        html
            (Animator.Css.div timeline
                [ Animator.Css.opacity <|
                    \indices ->
                        if accessCurrent indices == index then
                            Animator.at 1

                        else
                            Animator.at 0.0
                ]
                []
                [ pre
                    []
                    [ Html.text t ]
                ]
            )


emptyColumn =
    el
        [ width shrink
        , htmlAttribute (Attributes.style "line-height" "2")
        ]
    <|
        html
            (pre
                []
                [ Html.text " " ]
            )


iteration :
    Animator.Timeline PageIndices
    -> Int
    -> String
    -> String
    -> String
    -> String
    -> Element msg
iteration timeline index david gerhard luc ludvig =
    row [ centerX, centerY ]
        [ textColumn timeline .dp index (format david)
        , emptyColumn
        , textColumn timeline .ge index (format gerhard)
        , emptyColumn
        , textColumn timeline .ld index (format luc)
        , emptyColumn
        , textColumn timeline .le index (format ludvig)
        ]


repeatLastN : Int -> List String -> List String
repeatLastN n lst =
    let
        lack =
            n - List.length lst
    in
    if lack > 0 then
        case L.last lst of
            Just l ->
                lst ++ List.repeat n l

            Nothing ->
                lst ++ List.repeat n " "

    else
        lst


maxLength lsts =
    List.map List.length lsts
        |> List.maximum


padLists : List (List String) -> List (List String)
padLists lsts =
    maxLength lsts
        |> Maybe.map (\n -> List.map (repeatLastN n) lsts)
        |> Maybe.withDefault lsts


matryoshka : List (Element msg) -> Element msg
matryoshka els =
    case els of
        first :: rest ->
            el
                [ inFront (matryoshka rest)
                , width fill
                ]
                first

        [] ->
            none


buttons : PageIndices -> Int -> Element Msg
buttons indices maxIdx =
    row [ spacing 20, centerX ]
        [ Input.button
            [ Border.width 1, padding 10 ]
            { onPress =
                Just <|
                    SetPage
                        { ld = modBy maxIdx (indices.ld - 1)
                        , ge = modBy maxIdx (indices.ge - 1)
                        , dp = modBy maxIdx (indices.ge - 1)
                        , le = modBy maxIdx (indices.ge - 1)
                        }
            , label = text "Previous Page"
            }
        , Input.button
            [ Border.width 1, padding 10 ]
            { onPress =
                Just <|
                    SetPage
                        { ld = modBy maxIdx (indices.ld + 1)
                        , ge = modBy maxIdx (indices.ge + 1)
                        , dp = modBy maxIdx (indices.ge + 1)
                        , le = modBy maxIdx (indices.ge + 1)
                        }
            , label = text "Next Page"
            }
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Towards"
    , body =
        [ layout
            [ width fill
            , centerY
            , padding 20
            , Font.size 14
            , Font.family
                [ Font.monospace ]
            ]
          <|
            column [ width fill, spacingXY 0 50 ] <|
                [ matryoshka <|
                    List.indexedMap
                        (\i l ->
                            case l of
                                [ d, g, luc, ludvig ] ->
                                    iteration model.pageIndices i d g luc ludvig

                                _ ->
                                    let
                                        _ =
                                            Debug.log "l" l
                                    in
                                    text "Problem"
                        )
                        (padLists
                            [ David.texts
                            , Gerhard.texts
                            , Luc.texts
                            , Ludvig.texts
                            ]
                            |> L.transpose
                        )
                , buttons (Animator.current model.pageIndices)
                    (maxLength
                        [ Luc.texts
                        , Gerhard.texts
                        , David.texts
                        , Ludvig.texts
                        ]
                        |> Maybe.withDefault 0
                    )
                ]
        ]
    }
