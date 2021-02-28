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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (..)
import Html exposing (pre)
import Html.Attributes as Attributes
import Html.Events
import Html.Events.Extra.Wheel as Wheel
import List.Extra as L
import Luc.TextGen
import PageIndices exposing (Author(..), PageIndices)
import Pages
import Texts exposing (Entry, Texts)
import Texts.David as David
import Texts.Gerhard as Gerhard
import Texts.Intro as Intro
import Texts.Luc as Luc
import Texts.Ludvig as Ludvig
import Time
import Url exposing (Url)
import Url.Parser
import Utils


config =
    { scrollInc = 0.1, transitionDur = 1, transitionDepth = 1.0 }


type InitStatus
    = Uninitialized
    | WithAudio
    | WithoutAudio


withAudio : InitStatus -> Bool
withAudio s =
    case s of
        WithoutAudio ->
            False

        _ ->
            True


type alias Model =
    { indexGE : Animator.Timeline Float
    , indexLE : Animator.Timeline Float
    , indexLD : Animator.Timeline Float
    , indexDP : Animator.Timeline Float
    , rotation : Int
    , navKey : Nav.Key
    , needsUpdate : Bool
    , texts : Texts
    , initStatus : InitStatus
    , testMode : Bool
    , muteGE : Bool
    , muteLE : Bool
    , muteDP : Bool
    , muteLD : Bool
    }


modelIndices : Model -> PageIndices
modelIndices m =
    PageIndices.PageIndices
        (Animator.current m.indexLE)
        (Animator.current m.indexDP)
        (Animator.current m.indexGE)
        (Animator.current m.indexLD)
        m.rotation


muteOf : Author -> Model -> Bool
muteOf author m =
    case author of
        Gerhard ->
            m.muteGE

        Luc ->
            m.muteLD

        Ludvig ->
            m.muteLE

        David ->
            m.muteDP


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
                        , ld =
                            Luc.TextGen.generateEntries
                                (Luc.TextGen.Probabilities 0.6 0.35 0.0)
                                (Texts.textsToList Gerhard.texts)
                                (Texts.textsToList Ludvig.texts)
                        }
                in
                ( { indexGE = Animator.init (Pages.indices page).ge
                  , indexLD = Animator.init (Pages.indices page).ld
                  , indexLE = Animator.init (Pages.indices page).le
                  , indexDP = Animator.init (Pages.indices page).dp
                  , rotation = (Pages.indices page).rotation
                  , navKey = navKey
                  , needsUpdate = False
                  , texts = texts
                  , initStatus =
                        if not <| Pages.withAudio page then
                            WithoutAudio

                        else
                            Uninitialized
                  , testMode = Pages.testMode page
                  , muteLD = False
                  , muteGE = False
                  , muteLE = False
                  , muteDP = False
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ animatorLD
                        |> Animator.toSubscription Tick model
                    , animatorLE
                        |> Animator.toSubscription Tick model
                    , animatorDP
                        |> Animator.toSubscription Tick model
                    , animatorGE
                        |> Animator.toSubscription Tick model
                    , bufferLoaderCreated BufferLoaderCreated
                    ]
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }



--


port initAudio : List Float -> Cmd msg


port setAmps : ( Int, Array Float ) -> Cmd msg


port setPan : ( Int, Float ) -> Cmd msg


port sendMute : ( Int, Bool ) -> Cmd msg


port bufferLoaderCreated : (Bool -> msg) -> Sub msg



--


type Msg
    = Tick Time.Posix
    | ClickedLink Browser.UrlRequest
    | UrlChanged Url.Url
    | SetPage Author Float
    | SetEditor Author String
    | Scroll Float Author
    | SetMute Author Bool
    | BufferLoaderCreated Bool
    | InitAudio


incOrDec : Wheel.Event -> Float
incOrDec wheelEvent =
    if wheelEvent.deltaY > 0 then
        -1.0

    else
        1.0


ampsCmd model indices =
    case model.initStatus of
        WithAudio ->
            List.map
                (\a ->
                    setAmps
                        ( PageIndices.authorIndex a
                        , ampArray indices a (Texts.length model.texts)
                        )
                )
                PageIndices.authors
                |> Cmd.batch

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitAudio ->
            ( model
            , initAudio
                (PageIndices.indicesList (modelIndices model))
            )

        BufferLoaderCreated b ->
            let
                maxIdx =
                    Texts.length model.texts

                amps =
                    ampsCmd model (modelIndices model)

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
            ( { model | initStatus = WithAudio }, Cmd.batch (amps :: panCmds) )

        Scroll incDec author ->
            let
                newIndices =
                    PageIndices.incIndex author
                        (config.scrollInc * incDec)
                        (Texts.length model.texts)
                        (modelIndices model)
            in
            ( model
            , Nav.pushUrl model.navKey
                (Pages.toUrl
                    (Pages.root newIndices
                        (withAudio model.initStatus)
                        model.testMode
                    )
                )
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
                        (modelIndices model)
                        entry
                        model.texts
              }
            , Cmd.none
            )

        Tick newTime ->
            ( Animator.update newTime animatorLD model
                |> Animator.update newTime animatorGE
                |> Animator.update newTime animatorDP
                |> Animator.update newTime animatorLE
            , Cmd.none
            )

        ClickedLink _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            let
                page =
                    Pages.fromUrl url

                newIndices =
                    Pages.indices page

                amps =
                    ampsCmd model newIndices
            in
            ( { model
                | indexLD =
                    model.indexLD
                        |> Animator.go
                            (Animator.seconds config.transitionDur)
                            newIndices.ld
                , indexLE =
                    model.indexLE
                        |> Animator.go
                            (Animator.seconds config.transitionDur)
                            newIndices.le
                , indexGE =
                    model.indexGE
                        |> Animator.go
                            (Animator.seconds config.transitionDur)
                            newIndices.ge
                , indexDP =
                    model.indexDP
                        |> Animator.go
                            (Animator.seconds config.transitionDur)
                            newIndices.dp
              }
            , amps
            )

        SetPage author index ->
            ( model
            , Nav.pushUrl
                model.navKey
                (Pages.toUrl
                    (Pages.root
                        (PageIndices.updateAuthor
                            author
                            index
                            (modelIndices model)
                        )
                        (withAudio model.initStatus)
                        model.testMode
                    )
                )
            )

        SetMute author b ->
            let
                newModel =
                    case author of
                        Gerhard ->
                            { model | muteGE = b }

                        Ludvig ->
                            { model | muteLE = b }

                        Luc ->
                            { model | muteLD = b }

                        David ->
                            { model | muteDP = b }
            in
            ( newModel, sendMute ( PageIndices.authorIndex author, b ) )


animatorLD : Animator.Animator Model
animatorLD =
    Animator.animator
        |> Animator.Css.watching .indexLD
            (\newIndex model ->
                { model | indexLD = newIndex }
            )


animatorLE : Animator.Animator Model
animatorLE =
    Animator.animator
        |> Animator.Css.watching .indexLE
            (\newIndex model ->
                if newIndex == model.indexLE then
                    model

                else
                    { model | indexLE = newIndex }
            )


animatorDP : Animator.Animator Model
animatorDP =
    Animator.animator
        |> Animator.Css.watching .indexDP
            (\newIndex model ->
                if newIndex == model.indexDP then
                    model

                else
                    { model | indexDP = newIndex }
            )


animatorGE : Animator.Animator Model
animatorGE =
    Animator.animator
        |> Animator.Css.watching .indexGE
            (\newIndex model ->
                if newIndex == model.indexGE then
                    model

                else
                    { model | indexGE = newIndex }
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


textColumn :
    Bool
    -> Animator.Timeline Float
    -> Author
    -> Int
    -> Int
    -> Entry
    -> Element Msg
textColumn border timeline author index maxIdx entry =
    el
        ([ width shrink
         , htmlAttribute (Attributes.style "line-height" "2")
         ]
            ++ (if border then
                    [ Border.solid
                    , Border.width 1
                    ]

                else
                    []
               )
        )
    <|
        html
            (Animator.Css.div timeline
                [ Animator.Css.opacity <|
                    \currentIndex ->
                        calcDistance currentIndex index maxIdx
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
        , htmlAttribute (Attributes.style "line-height" "1")
        ]
    <|
        html
            (pre
                []
                [ Html.text (String.repeat n " ") ]
            )


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


muteButtonStyling b =
    if b then
        Background.color (rgb 0.6 0.6 0.6) :: buttonStyling

    else
        buttonStyling


muteButton : Author -> Bool -> Element Msg
muteButton author state =
    Input.button (muteButtonStyling state)
        { onPress =
            Just <|
                SetMute author (not state)
        , label = text "Mute"
        }


columnButtons : Author -> Int -> Float -> Element Msg
columnButtons author maxIdx index =
    let
        back =
            PageIndices.previousIndex maxIdx index

        forward =
            PageIndices.nextIndex maxIdx index
    in
    row [ centerX, spacing 10 ]
        [ Input.button buttonStyling
            { onPress =
                Just <|
                    SetPage author back
            , label = text "<"
            }
        , text (String.fromFloat index)
        , Input.button buttonStyling
            { onPress =
                Just <|
                    SetPage author forward
            , label = text ">"
            }
        ]


buttons : Model -> Element Msg
buttons model =
    let
        maxIdx =
            Texts.length model.texts

        indices =
            modelIndices model
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
                (List.map
                    (\author ->
                        column [ spacingXY 0 10, centerX ]
                            [ columnButtons author
                                maxIdx
                                (PageIndices.getIndex author indices)
                            , if model.testMode then
                                muteButton author (muteOf author model)

                              else
                                none
                            ]
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
            modelIndices m

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
            (Utils.rotate m.rotation
                [ editColumn david (SetEditor David)
                , editColumn gerhard (SetEditor Gerhard)
                , editColumn luc (SetEditor Luc)
                , editColumn ludvig (SetEditor Ludvig)
                ]
            )


introPage : Element Msg
introPage =
    column [ width fill, spacing 20 ]
        [ Intro.texts
        , Input.button [ padding 10, Border.width 1 ]
            { onPress = Just InitAudio, label = text "Init audio" }
        ]


viewColumns : Model -> Element Msg
viewColumns model =
    let
        maxIdx =
            Texts.length model.texts

        authorTexts author entries timeline =
            let
                currInterpolationIdx =
                    Animator.current timeline
            in
            List.indexedMap
                (\i e ->
                    let
                        dist =
                            calcDistance currInterpolationIdx i maxIdx
                    in
                    ( dist, textColumn model.testMode timeline author i maxIdx e )
                )
                entries
                |> List.filterMap
                    (\( dist, column ) ->
                        if dist < (config.transitionDepth * 2) then
                            Just column

                        else
                            Nothing
                    )
                |> matryoshka
    in
    column [ width fill, spacingXY 0 5 ] <|
        [ row [ centerX, centerY ] <|
            List.intersperse (emptyColumn 1) <|
                Utils.rotate model.rotation
                    [ lazy3 authorTexts David model.texts.dp model.indexDP
                    , lazy3 authorTexts Gerhard model.texts.ge model.indexGE
                    , lazy3 authorTexts Luc model.texts.ld model.indexLD
                    , lazy3 authorTexts Ludvig model.texts.le model.indexLE
                    ]
        , buttons model
        , if model.testMode then
            viewEditable model

          else
            none
        ]


viewMainContent : Model -> Element Msg
viewMainContent model =
    case model.initStatus of
        Uninitialized ->
            introPage

        _ ->
            viewColumns model


view : Model -> Browser.Document Msg
view model =
    { title = "Towards"
    , body =
        [ layout
            [ width fill
            , centerY
            , padding 20
            , Font.size 13
            , Font.family
                [ Font.typeface "Inconsolata", Font.monospace ]
            ]
          <|
            viewMainContent model
        ]
    }
