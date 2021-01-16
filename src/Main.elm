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
import Html.Events
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
    , texts : { ge : List String, dp : List String, ld : List String, le : List String }
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
                  , texts =
                        { ge = List.map format Gerhard.texts
                        , le = List.map format Ludvig.texts
                        , dp = List.map format David.texts
                        , ld = List.map format Luc.texts
                        }
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
    | SetEditorGerhard String
    | SetEditorLudvig String
    | SetEditorDavid String
    | SetEditorLuc String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEditorGerhard str ->
            let
                oldTexts =
                    model.texts

                _ =
                    Debug.log "Text:" (formatPrinting str)

                t =
                    L.setAt (Animator.current model.pageIndices).ge str model.texts.ge

                newTexts =
                    { oldTexts | ge = t }
            in
            ( { model | texts = newTexts }, Cmd.none )

        SetEditorDavid str ->
            let
                oldTexts =
                    model.texts

                _ =
                    Debug.log "Text:" (formatPrinting str)

                t =
                    L.setAt (Animator.current model.pageIndices).dp str model.texts.dp

                newTexts =
                    { oldTexts | dp = t }
            in
            ( { model | texts = newTexts }, Cmd.none )

        SetEditorLuc str ->
            let
                oldTexts =
                    model.texts

                _ =
                    Debug.log "Text:" (formatPrinting str)

                t =
                    L.setAt (Animator.current model.pageIndices).ld str model.texts.ld

                newTexts =
                    { oldTexts | ld = t }
            in
            ( { model | texts = newTexts }, Cmd.none )

        SetEditorLudvig str ->
            let
                oldTexts =
                    model.texts

                _ =
                    Debug.log "Text:" (formatPrinting str)

                t =
                    L.setAt (Animator.current model.pageIndices).le str model.texts.le

                newTexts =
                    { oldTexts | le = t }
            in
            ( { model | texts = newTexts }, Cmd.none )

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
        |> String.replace "\n" ""
        |> String.left (40 * 30)
        |> String.padRight (40 * 30) ' '
        |> insertNewlinesEveryN 40


formatPrinting : String -> String
formatPrinting s =
    s
        |> String.replace "\n" ""
        |> String.left (40 * 30)
        |> String.padRight (40 * 30) ' '


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
                [ pre []
                    [ Html.text t ]
                ]
            )


emptyColumn n =
    el
        [ width shrink
        , htmlAttribute (Attributes.style "line-height" "2")
        ]
    <|
        html
            (pre
                []
                [ Html.text (String.repeat n " ") ]
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
    row [ centerX, centerY ] <|
        List.intersperse (emptyColumn 1)
            [ textColumn timeline .dp index (format david)
            , textColumn timeline .ge index (format gerhard)
            , textColumn timeline .ld index (format luc)
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


buttonStyling =
    [ Border.width 1, padding 10, centerX ]


columnButtons : PageIndices -> PageIndices -> Int -> Element Msg
columnButtons back forward idx =
    row [ centerX, spacing 10 ]
        [ Input.button buttonStyling
            { onPress =
                Just <|
                    SetPage back
            , label = text "<"
            }
        , text (String.fromInt (idx + 1))
        , Input.button buttonStyling
            { onPress =
                Just <|
                    SetPage forward
            , label = text ">"
            }
        ]


buttons : PageIndices -> Int -> Element Msg
buttons indices maxIdx =
    row [ centerX ] <|
        List.intersperse (emptyColumn 1) <|
            List.map
                (\b ->
                    column [ width shrink ]
                        [ emptyColumn 40
                        , b
                        ]
                )
                [ columnButtons
                    { indices
                        | dp = modBy maxIdx (indices.dp - 1)
                    }
                    { indices
                        | dp = modBy maxIdx (indices.dp + 1)
                    }
                    indices.dp
                , columnButtons
                    { indices
                        | ge = modBy maxIdx (indices.ge - 1)
                    }
                    { indices
                        | ge = modBy maxIdx (indices.ge + 1)
                    }
                    indices.ge
                , columnButtons
                    { indices
                        | ld = modBy maxIdx (indices.ld - 1)
                    }
                    { indices
                        | ld = modBy maxIdx (indices.ld + 1)
                    }
                    indices.ld
                , columnButtons
                    { indices
                        | le = modBy maxIdx (indices.le - 1)
                    }
                    { indices
                        | le = modBy maxIdx (indices.le + 1)
                    }
                    indices.le
                ]


editColumn : String -> (String -> msg) -> Element msg
editColumn content msg =
    Input.multiline [ width (px 370), height (px 833), htmlAttribute (Attributes.style "line-height" "2"), clip ]
        { onChange = msg
        , text = content
        , placeholder = Nothing
        , label = Input.labelHidden ""
        , spellcheck = True
        }


viewEditable : Model -> Element Msg
viewEditable m =
    let
        indices =
            Animator.current m.pageIndices

        david =
            Maybe.withDefault " " <| L.getAt indices.dp m.texts.dp

        gerhard =
            Maybe.withDefault " " <| L.getAt indices.ge m.texts.ge

        luc =
            Maybe.withDefault " " <| L.getAt indices.ld m.texts.ld

        ludvig =
            Maybe.withDefault " " <| L.getAt indices.le m.texts.le
    in
    row [ centerX, centerY ] <|
        List.intersperse (emptyColumn 1)
            [ editColumn david SetEditorDavid
            , editColumn gerhard SetEditorGerhard
            , editColumn luc SetEditorLuc
            , editColumn ludvig SetEditorLudvig
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
            column [ width fill, spacingXY 0 20 ] <|
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
                            [ model.texts.dp
                            , model.texts.ge
                            , model.texts.ld
                            , model.texts.le
                            ]
                            |> L.transpose
                        )
                , buttons (Animator.current model.pageIndices)
                    (maxLength
                        [ model.texts.dp
                        , model.texts.ge
                        , model.texts.ld
                        , model.texts.le
                        ]
                        |> Maybe.withDefault 0
                    )
                , viewEditable model
                ]
        ]
    }
