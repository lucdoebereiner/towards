port module Main exposing (main)

import Animator
import Animator.Css
import Animator.Inline
import Array exposing (Array)
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
import Pages
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
    , audioInitialized : Bool
    }


main =
    Browser.application
        { init =
            \() url navKey ->
                let
                    page =
                        Pages.fromUrl url

                    texts =
                        { ge = Texts.textsToList Gerhard.texts
                        , le = Texts.textsToList Ludvig.texts
                        , dp = Texts.textsToList David.texts
                        , ld = Texts.textsToList Luc.texts
                        }
                in
                ( { pageIndices =
                        Animator.init (Pages.indices page)
                  , navKey = navKey
                  , needsUpdate = False
                  , texts = texts
                  , audioInitialized = False
                  }
                , Cmd.none
                  -- , if Pages.withAudio page then
                  --     initAudio ()
                  --   else
                  --     Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ animator
                        |> Animator.toSubscription Tick model
                    , bufferLoaderCreated BufferLoaderCreated
                    ]
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }



--


port initAudio : () -> Cmd msg


port setAmps : ( Int, Array Float ) -> Cmd msg


port setPan : ( Int, Float ) -> Cmd msg


port bufferLoaderCreated : (Bool -> msg) -> Sub msg



--


type Msg
    = Tick Time.Posix
    | ClickedLink Browser.UrlRequest
    | UrlChanged Url.Url
    | SetPage PageIndices
    | SetEditor Author String
    | Scroll Float Author
    | BufferLoaderCreated Bool
    | InitAudio


incOrDec : Wheel.Event -> Float
incOrDec wheelEvent =
    if wheelEvent.deltaY > 0 then
        -1.0

    else
        1.0


ampsCmd model indices =
    List.map
        (\a ->
            setAmps
                ( PageIndices.authorIndex a
                , ampArray indices a (Texts.length model.texts)
                )
        )
        PageIndices.authors
        |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitAudio ->
            ( { model | audioInitialized = True }, initAudio () )

        BufferLoaderCreated b ->
            let
                _ =
                    Debug.log "buffer loader created" b

                maxIdx =
                    Texts.length model.texts

                indices =
                    Animator.current model.pageIndices

                amps =
                    ampsCmd model indices

                panCmds =
                    List.map
                        (\author ->
                            setPan
                                ( PageIndices.authorIndex author
                                , authorPan author
                                )
                        )
                        PageIndices.authors
            in
            ( model, Cmd.batch (amps :: panCmds) )

        Scroll incDec author ->
            let
                newIndices =
                    PageIndices.incIndex author
                        (config.scrollInc * incDec)
                        (Texts.length model.texts)
                        (Animator.current model.pageIndices)

                -- amps =
                --     ampsCmd model newIndices
            in
            ( model
            , --Cmd.batch
              Nav.pushUrl model.navKey (PageIndices.toUrl newIndices)
              -- , amps
              -- ]
            )

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
            let
                newIndices =
                    PageIndices.fromUrl url

                amps =
                    ampsCmd model newIndices
            in
            ( { model
                | pageIndices =
                    model.pageIndices
                        |> Animator.go
                            (Animator.seconds config.transitionDur)
                            newIndices
              }
            , amps
            )

        SetPage newIndices ->
            -- let
            --     amps =
            --         ampsCmd model newIndices
            -- in
            ( model
            , Nav.pushUrl
                model.navKey
                (PageIndices.toUrl newIndices)
              -- , amps
              -- ]
            )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching .pageIndices
            (\newIndices model ->
                { model | pageIndices = newIndices }
            )



-- opacity, panning and amplitudes


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


authorPan : Author -> Float
authorPan author =
    -0.7 + ((1.4 / 3) * PageIndices.authorIndex author)


ampArray : PageIndices -> Author -> Int -> Array Float
ampArray indices author maxIdx =
    let
        authorIdx =
            PageIndices.getIndex author indices
    in
    Array.initialize maxIdx (\i -> calcDistance authorIdx i maxIdx |> distanceToOpacity)



-- panAndAmp : PageIndices -> Author -> Int -> ( Float, Array Float )
-- panAndAmp indices author maxIdx =
--     ( authorPan author, ampArray indices author maxIdx )
--


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
    -> Texts.CurrentEntries
    -> Element Msg
iteration timeline index maxIdx current =
    row [ centerX, centerY ] <|
        List.intersperse (emptyColumn 1) <|
            List.map
                (\author ->
                    textColumn timeline
                        author
                        index
                        maxIdx
                        (Texts.getText author current)
                )
                (PageIndices.authorsInOrder (Animator.current timeline))


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
                (List.map
                    (\author ->
                        columnButtons author
                            maxIdx
                            indices
                    )
                    (PageIndices.authorsInOrder indices)
                )


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
                                        { ld = luc, dp = d, ge = g, le = ludvig }

                                _ ->
                                    let
                                        _ =
                                            Debug.log "l" l
                                    in
                                    text "Problem"
                        )
                        (Texts.transposedTexts model.texts)
                , buttons (Animator.current model.pageIndices) (Texts.length model.texts)
                , if not model.audioInitialized then
                    Input.button [] { onPress = Just InitAudio, label = text "Init audio" }

                  else
                    none
                , viewEditable model
                ]
        ]
    }
