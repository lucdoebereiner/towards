module Main exposing (main)

import Animator
import Animator.Css
import Animator.Inline
import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (pre)
import Html.Attributes as Attributes
import Html.Events
import Html.Events.Extra.Wheel as Wheel
import List.Extra as L
import PageIndices exposing (Author(..), PageIndices)
import Texts exposing (Entry, Texts)
import Texts.David as David
import Texts.Gerhard as Gerhard
import Texts.Luc as Luc
import Texts.Ludvig as Ludvig
import Time
import Url exposing (Url)
import Url.Parser


config =
    { scrollInc = 0.1, transitionDur = 1, transitionDepth = 1.0 }


type alias Model =
    { pageIndices : Animator.Timeline PageIndices
    , navKey : Nav.Key
    , needsUpdate : Bool
    , texts : Texts
    }


parseIndices : Url -> PageIndices
parseIndices url =
    let
        _ =
            Debug.log "url" url

        _ =
            Debug.log "parse result" (Url.Parser.parse PageIndices.parsePageIndices url)
    in
    Maybe.withDefault
        PageIndices.default
        (Url.Parser.parse PageIndices.parsePageIndices url)


main =
    Browser.application
        { init =
            \() url navKey ->
                ( { pageIndices =
                        Animator.init (parseIndices url)
                  , navKey = navKey
                  , needsUpdate = False
                  , texts =
                        { ge = Texts.textsToList Gerhard.texts
                        , le = Texts.textsToList Ludvig.texts
                        , dp = Texts.textsToList David.texts
                        , ld = Texts.textsToList Luc.texts
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
    | Scroll Float Author


incOrDec : Wheel.Event -> Float
incOrDec wheelEvent =
    if wheelEvent.deltaY > 0 then
        -1.0

    else
        1.0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scroll incDec author ->
            let
                newIndices =
                    PageIndices.incIndex author
                        (config.scrollInc * incDec)
                        (Texts.length model.texts)
                        (Animator.current model.pageIndices)
            in
            ( model, Nav.pushUrl model.navKey (PageIndices.toUrl newIndices) )

        SetEditor author str ->
            let
                entry =
                    Texts.fromEditor str

                _ =
                    Debug.log "Text:" (Texts.printEntry entry)
            in
            ( { model
                | texts =
                    Texts.setAuthorTextAt
                        author
                        (Animator.current model.pageIndices)
                        entry
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

        UrlChanged url ->
            ( { model
                | pageIndices =
                    model.pageIndices
                        |> Animator.go
                            (Animator.seconds config.transitionDur)
                            (parseIndices url)
              }
            , Cmd.none
            )

        SetPage newIndices ->
            ( model, Nav.pushUrl model.navKey (PageIndices.toUrl newIndices) )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching .pageIndices
            (\newIndices model ->
                { model | pageIndices = newIndices }
            )


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
    if d >= config.transitionDepth then
        0.0

    else
        (config.transitionDepth - d) / config.transitionDepth


textColumn :
    Animator.Timeline PageIndices
    -> Author
    -> Int
    -> Int
    -> Entry
    -> Element Msg
textColumn timeline author index maxIdx entry =
    el
        [ width shrink
        , htmlAttribute (Attributes.style "line-height" "2")
        ]
    <|
        html
            (Animator.Css.div timeline
                [ Animator.Css.opacity <|
                    \indices ->
                        calcDistance (PageIndices.getIndex author indices) index maxIdx
                            |> distanceToOpacity
                            |> Animator.at
                ]
                []
                [ pre [ Wheel.onWheel (\ev -> Scroll (incOrDec ev) author) ]
                    [ Html.text (Texts.entryString entry) ]
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
    -> Entry
    -> Entry
    -> Entry
    -> Entry
    -> Element Msg
iteration timeline index maxIdx david gerhard luc ludvig =
    row [ centerX, centerY ] <|
        List.intersperse (emptyColumn 1)
            [ textColumn timeline David index maxIdx david
            , textColumn timeline Gerhard index maxIdx gerhard
            , textColumn timeline Luc index maxIdx luc
            , textColumn timeline Ludvig index maxIdx ludvig
            ]


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


columnButtons : Author -> Int -> PageIndices -> Element Msg
columnButtons author maxIdx indices =
    let
        back =
            PageIndices.previousIndex author maxIdx indices

        forward =
            PageIndices.nextIndex author maxIdx indices

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
    row [ centerX ] <|
        List.intersperse (emptyColumn 1) <|
            List.map
                (\b ->
                    column [ width shrink ]
                        [ emptyColumn 40
                        , b
                        ]
                )
                [ columnButtons David maxIdx indices
                , columnButtons Gerhard maxIdx indices
                , columnButtons Luc maxIdx indices
                , columnButtons Ludvig maxIdx indices
                ]


editColumn : String -> (String -> msg) -> Element msg
editColumn content msg =
    Input.multiline
        [ width (px 370)
        , height (px 833)
        , htmlAttribute
            (Attributes.style "line-height" "2")
        , clip
        , htmlAttribute (Attributes.cols 40)
        , htmlAttribute (Attributes.rows 30)
        , htmlAttribute (Attributes.wrap "hard")
        ]
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

        extractString =
            Maybe.withDefault " " << Maybe.map Texts.toEditor

        david =
            Texts.indexTexts David indices m.texts
                |> extractString

        gerhard =
            Texts.indexTexts Gerhard indices m.texts
                |> extractString

        luc =
            Texts.indexTexts Luc indices m.texts
                |> extractString

        ludvig =
            Texts.indexTexts Ludvig indices m.texts
                |> extractString
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
                                    iteration model.pageIndices
                                        i
                                        (Texts.length model.texts)
                                        d
                                        g
                                        luc
                                        ludvig

                                _ ->
                                    let
                                        _ =
                                            Debug.log "l" l
                                    in
                                    text "Problem"
                        )
                        (Texts.transposedTexts model.texts)
                , buttons (Animator.current model.pageIndices) (Texts.length model.texts)
                , viewEditable model
                ]
        ]
    }
