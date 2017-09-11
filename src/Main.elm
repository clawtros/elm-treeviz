module Main exposing (..)

import Html exposing (Html, text, div, img)
import Random
import Process
import Task
import Time
import Svg exposing (Svg, circle, g, text_, line)
import Svg.Attributes as SvgAttributes


---- MODEL ----


type Color
    = Red
    | Black


type Tree comparable
    = EmptyTree
    | Node Color (Tree comparable) comparable (Tree comparable)


type alias Model =
    { tree : Tree Int }


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (always msg)



-- T B a z b
-- where
-- T _ a z b = ins s
-- ins s@(T B a y b)
-- 	| x<y = balance (ins a) y b
-- 	| x>y = balance a y (ins b)
-- 	| otherwise = s
-- ins s@(T R a y b)
-- 	| x<y = T R (ins a) y b
-- 	| x>y = T R a y (ins b)
-- 	| otherwise = s


insertTree : Tree comparable -> comparable -> Tree comparable
insertTree tree val =
    case tree of
        EmptyTree ->
            Node Black EmptyTree val EmptyTree

        Node Red left nodeVal right ->
            if val < nodeVal then
                balance (insertTree left val) nodeVal right
            else if val > nodeVal then
                balance left nodeVal (insertTree right val)
            else
                tree

        Node Black left nodeVal right ->
            if val < nodeVal then
                Node Red (insertTree left val) nodeVal right
            else if val > nodeVal then
                Node Red left nodeVal (insertTree right val)
            else
                tree


balance : Tree comparable -> comparable -> Tree comparable -> Tree comparable
balance left val right =
    let
        eeee =
            Debug.log "l,r" ( left, right )
    in
        case ( left, right ) of
            -- balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
            ( Node Red a x b, Node Red c z d ) ->
                Debug.log "a" <| Node Red (Node Black a x b) val (Node Black c z d)

            -- balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
            ( Node Red (Node Red a x b) y c, _ ) ->
                Debug.log "b" <| Node Red (Node Black a x b) y (Node Black c val right)

            -- balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
            ( Node Red a x (Node Red b y c), _ ) ->
                Debug.log "c" <| Node Red (Node Black a x b) y (Node Black c val right)

            -- balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
            ( _, Node Red b y (Node Red c z d) ) ->
                Debug.log "d" <| Node Red (Node Black left val b) y (Node Black c z d)

            -- balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
            ( _, Node Red (Node Red b y c) z d ) ->
                Debug.log "e" <| Node Red (Node Black left val b) y (Node Black c z d)

            -- balance a x b = T B a x b
            ( l, r ) ->
                Debug.log "anything" <| Node Red l val r


generator : Random.Generator Int
generator =
    Random.int 0 100


generateRandom : Cmd Msg
generateRandom =
    Random.generate InsertNumber generator


init : ( Model, Cmd Msg )
init =
    ( { tree = EmptyTree }, delay 10 GenerateNumber )



---- UPDATE ----


type Msg
    = NoOp
    | InsertNumber Int
    | GenerateNumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertNumber n ->
            { model | tree = insertTree model.tree n }
                ! [ delay 250 GenerateNumber
                  ]

        GenerateNumber ->
            model ! [ generateRandom ]

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


viewTree : Tree comparable -> Int -> Int -> Int -> Svg.Svg Msg
viewTree tree px py depth =
    let
        radius =
            "8"

        fd =
            toFloat depth + 1

        d =
            round <| 100 / (fd ^ 2) * 1.2 + 5

        ny =
            py + 50

        rx =
            px + d

        lx =
            px - d

        slx =
            toString lx

        srx =
            toString rx

        sny =
            toString ny
    in
        case tree of
            EmptyTree ->
                text ""

            Node color left val right ->
                g []
                    [ line
                        [ SvgAttributes.x1 <| toString px
                        , SvgAttributes.y1 <| toString py
                        , SvgAttributes.x2 slx
                        , SvgAttributes.y2 sny
                        , SvgAttributes.style "stroke: rgb(128,128,128); stroke-width: 1"
                        ]
                        []
                    , line
                        [ SvgAttributes.x1 <| toString px
                        , SvgAttributes.y1 <| toString py
                        , SvgAttributes.x2 srx
                        , SvgAttributes.y2 sny
                        , SvgAttributes.style "stroke: rgb(64,64,64); stroke-width: 1"
                        ]
                        []
                    , circle
                        [ SvgAttributes.r radius
                        , SvgAttributes.cx <| toString px
                        , SvgAttributes.cy <| toString py
                        , SvgAttributes.style <|
                            "stroke: black;"
                                ++ if color == Red then
                                    "fill: red; color: black"
                                   else
                                    "fill: black; color: white"
                        ]
                        []
                    , text_
                        [ SvgAttributes.transform <| "translate(" ++ (toString <| px - 4) ++ "," ++ (toString <| py + 4) ++ ")"
                        , SvgAttributes.style <|
                            "font-size:8px; font-family: sans-serif;"
                                ++ if color == Red then
                                    "fill: black"
                                   else
                                    "fill: white"
                        ]
                        [ text <| toString val ]
                    , viewTree left lx ny (depth + 1)
                    , viewTree right rx ny (depth + 1)
                    ]


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg [ SvgAttributes.width "800", SvgAttributes.height "800" ]
            [ viewTree model.tree 400 10 0
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
