module Main exposing (main)

import Animator
import Animator.Css
import Animator.Inline
import Basics.Extra exposing (fractionalModBy)
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
import PageIndices exposing (Author(..), PageIndices)
import Texts exposing (Texts)
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
    , texts : Texts
    }


main =
    Browser.application
        { init =
            \() url navKey ->
                ( { pageIndices = Animator.init PageIndices.default
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
    | SetEditor Author String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEditor author str ->
            let
                _ =
                    Debug.log "Text:" (formatPrinting str)
            in
            ( { model
                | texts =
                    Texts.setAuthorTextAt
                        author
                        (Animator.current model.pageIndices)
                        str
                        model.texts
              }
            , Cmd.none
            )

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
                        |> Animator.go Animator.immediately
                            --(Animator.seconds 5)
                            newIndices
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


calcDistance : Float -> Int -> Int -> Float
calcDistance currentIdx textIdx maxIdx =
    let
        distance =
            abs (toFloat textIdx - currentIdx)

        reverseDistance =
            toFloat maxIdx - distance
    in
    min distance reverseDistance


distanceToOpacity : Float -> Float
distanceToOpacity d =
    if d >= 1.0 then
        0.0

    else
        1.0 - d


textColumn :
    Animator.Timeline PageIndices
    -> Author
    -> Int
    -> Int
    -> String
    -> Element msg
textColumn timeline author index maxIdx t =
    el
        [ width shrink
        , htmlAttribute (Attributes.style "line-height" "2")
        ]
    <|
        html
            (Animator.Css.div timeline
                [ Animator.Css.opacity <|
                    \indices ->
                        let
                            distance =
                                calcDistance (PageIndices.getIndex author indices) index maxIdx

                            opacity =
                                distanceToOpacity distance

                            _ =
                                Debug.log "page index" (PageIndices.getIndex author indices)

                            _ =
                                Debug.log "maxidx" maxIdx

                            _ =
                                Debug.log "dist, index opacity" ( distance, index, opacity )
                        in
                        Animator.at opacity
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
    -> Int
    -> String
    -> String
    -> String
    -> String
    -> Element msg
iteration timeline index maxIdx david gerhard luc ludvig =
    row [ centerX, centerY ] <|
        List.intersperse (emptyColumn 1)
            [ textColumn timeline David index maxIdx (format david)
            , textColumn timeline Gerhard index maxIdx (format gerhard)
            , textColumn timeline Luc index maxIdx (format luc)
            , textColumn timeline Ludvig index maxIdx (format ludvig)
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


columnButtons : Author -> Int -> Float -> PageIndices -> Element Msg
columnButtons author maxIdx inc indices =
    let
        back =
            PageIndices.incIndex author (inc * -1.0) maxIdx indices

        forward =
            PageIndices.incIndex author inc maxIdx indices

        idx =
            PageIndices.getIndex author indices
    in
    row [ centerX, spacing 10 ]
        [ Input.button buttonStyling
            { onPress =
                Just <|
                    SetPage back
            , label = text "<"
            }
        , text (String.fromFloat idx)
        , Input.button buttonStyling
            { onPress =
                Just <|
                    SetPage forward
            , label = text ">"
            }
        ]


buttons : PageIndices -> Int -> Element Msg
buttons indices maxIdx =
    let
        inc =
            0.5
    in
    row [ centerX ] <|
        List.intersperse (emptyColumn 1) <|
            List.map
                (\b ->
                    column [ width shrink ]
                        [ emptyColumn 40
                        , b
                        ]
                )
                [ columnButtons David maxIdx inc indices
                , columnButtons Gerhard maxIdx inc indices
                , columnButtons Luc maxIdx inc indices
                , columnButtons Ludvig maxIdx inc indices
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
            Texts.indexTexts David indices m.texts

        gerhard =
            Texts.indexTexts Gerhard indices m.texts

        luc =
            Texts.indexTexts Luc indices m.texts

        ludvig =
            Texts.indexTexts Ludvig indices m.texts
    in
    row [ centerX, centerY ] <|
        List.intersperse (emptyColumn 1)
            [ editColumn david (SetEditor David)
            , editColumn gerhard (SetEditor Gerhard)
            , editColumn luc (SetEditor Luc)
            , editColumn ludvig (SetEditor Ludvig)
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
                                    iteration model.pageIndices i (Texts.length model.texts) d g luc ludvig

                                _ ->
                                    let
                                        _ =
                                            Debug.log "l" l
                                    in
                                    text "Problem"
                        )
                        -- todo: make opaque and move to texts module
                        (padLists
                            [ model.texts.dp
                            , model.texts.ge
                            , model.texts.ld
                            , model.texts.le
                            ]
                            |> L.transpose
                        )
                , buttons (Animator.current model.pageIndices) (Texts.length model.texts)
                , viewEditable model
                ]
        ]
    }
