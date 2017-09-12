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


blacken : Tree comparable -> Tree comparable
blacken tree =
    case tree of
        Node _ l v r ->
            Node Black l v r

        EmptyTree ->
            EmptyTree



{- Insertion and membership test as by Okasaki -}
-- insert :: Ord a => a -> RB a -> RB a
-- insert x s =
-- 	T B a z b
-- 	where
-- 	T _ a z b = ins s
-- 	ins E = T R E x E
-- 	ins s@(T B a y b)
-- 		| x<y = balance (ins a) y b
-- 		| x>y = balance a y (ins b)
-- 		| otherwise = s
-- 	ins s@(T R a y b)
-- 		| x<y = T R (ins a) y b
-- 		| x>y = T R a y (ins b)
-- 		| otherwise = s


insertTree : Tree comparable -> comparable -> Tree comparable
insertTree s val =
    let
        ins : Tree comparable -> Tree comparable
        ins tree =
            case tree of
                EmptyTree ->
                    Node Red EmptyTree val EmptyTree

                Node Black left nodeVal right ->
                    if val < nodeVal then
                        balance (ins left) nodeVal right
                    else if val > nodeVal then
                        balance left nodeVal (ins right)
                    else
                        tree

                Node Red left nodeVal right ->
                    if val < nodeVal then
                        Node Red (ins left) nodeVal right
                    else if val > nodeVal then
                        Node Red left nodeVal (ins right)
                    else
                        tree
    in
        blacken <| ins s


balance : Tree comparable -> comparable -> Tree comparable -> Tree comparable
balance left val right =
    case ( left, val, right ) of
        -- BOTH RED
        -- balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
        ( Node Red a x b, y, Node Red c z d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        -- DOUBLE RED LEFT
        -- balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
        ( Node Red (Node Red a x b) y c, z, d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        -- balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
        ( Node Red a x (Node Red b y c), z, d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        -- DOUBLE RED RIGHT
        -- balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
        ( a, x, Node Red b y (Node Red c z d) ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        -- balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
        ( a, x, Node Red (Node Red b y c) z d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        -- balance a x b = T B a x b
        ( a, x, b ) ->
            Node Black a x b


generator : Random.Generator Int
generator =
    Random.int -1000 1000


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
                ! [ delay 25 GenerateNumber
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
            round <| 300 / (fd ^ 1.5)

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
                        [ SvgAttributes.transform <|
                            "translate("
                                ++ (toString px)
                                ++ ","
                                ++ (toString <| py + 20)
                                ++ ")"
                        , SvgAttributes.style
                            "font-size:12px; font-family: sans-serif; text-anchor: middle;fill: black"
                        ]
                        [ text <| toString val ]
                    , viewTree left lx ny (depth + 1)
                    , viewTree right rx ny (depth + 1)
                    ]


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg [ SvgAttributes.width "1400", SvgAttributes.height "800" ]
            [ viewTree model.tree 700 10 0
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
