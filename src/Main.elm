module Main exposing (..)

import Browser exposing (element)
import Html exposing (Html, text, div)
import Html.Attributes as HA
import Random
import Process
import Task
import Time
import Svg exposing (Svg, circle, g, text_, line, rect)
import Svg.Keyed
import Svg.Attributes as SvgAttributes
import String


---- MODEL ----


type Color
    = Red
    | Black
    | BlackBlack


type Tree comparable
    = EmptyTree
    | Node Color (Tree comparable) comparable (Tree comparable)


type alias Model =
    { tree : Tree Int }


delay : Float -> msg -> Cmd msg
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


flatten : Tree comparable -> List comparable
flatten tree =
    case tree of
        EmptyTree ->
            []

        Node _ left val right ->
            flatten left ++ [ val ] ++ flatten right


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

                Node BlackBlack _ _ _ ->
                    tree
    in
        blacken <| ins s


batchInsert : Tree comparable -> List comparable -> Tree comparable
batchInsert t l =
    List.foldr (\a b -> insertTree b a) t l


balance : Tree comparable -> comparable -> Tree comparable -> Tree comparable
balance left val right =
    case ( left, val, right ) of
        ( Node Red a x b, y, Node Red c z d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        ( Node Red (Node Red a x b) y c, z, d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        ( Node Red a x (Node Red b y c), z, d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        ( a, x, Node Red b y (Node Red c z d) ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        ( a, x, Node Red (Node Red b y c) z d ) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        ( a, x, b ) ->
            Node Black a x b


generator : Random.Generator Int
generator =
    Random.int -100 100


generateRandom : Cmd Msg
generateRandom =
    Random.generate InsertNumber generator


init : ( Model, Cmd Msg )
init =
    ( { tree = EmptyTree }, generateRandom )



---- UPDATE ----


type Msg
    = NoOp
    | InsertNumber Int
    | GenerateNumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertNumber n ->
            ( { model | tree = insertTree model.tree n }
            , delay 100 GenerateNumber
            )

        GenerateNumber ->
            ( model, generateRandom )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


viewTree : Tree comparable -> Int -> Int -> Int -> List ( String, Svg.Svg Msg )
viewTree tree px py depth =
    let
        radius =
            max 2 <| 16 - (round <| (toFloat depth) * 1.5)

        fd =
            toFloat depth + 1

        d =
            round <| 300 / (fd ^ 1.6)

        ny =
            py + 60

        rx =
            px + d

        lx =
            px - d

        slx =
            String.fromInt <| lx - px

        srx =
            String.fromInt <| rx - px

        sny =
            String.fromInt <| ny - py
    in
        case tree of
            EmptyTree ->
                [ ( "", Svg.text "" ) ]

            Node color left val right ->
                ( Debug.toString val
                , g
                    [ SvgAttributes.style <| "transform: translate("
                        ++ String.fromInt px
                        ++ "px,"
                        ++ String.fromInt py
                        ++ "px)"
                    ]
                    [ line
                        [ SvgAttributes.x1 "0"
                        , SvgAttributes.y1 "0"
                        , SvgAttributes.x2 slx
                        , SvgAttributes.y2 sny
                        , SvgAttributes.style "stroke: rgb(128,128,128); stroke-width: 1"
                        ]
                        []
                    , line
                        [ SvgAttributes.x1 "0"
                        , SvgAttributes.y1 "0"
                        , SvgAttributes.x2 srx
                        , SvgAttributes.y2 sny
                        , SvgAttributes.style "stroke: rgb(64,64,64); stroke-width: 1"
                        ]
                        []
                    , circle
                        [ SvgAttributes.r <| String.fromInt radius
                        , SvgAttributes.cx "0"
                        , SvgAttributes.cy "0"
                        , SvgAttributes.style <|
                            "stroke: black; fill:"
                                ++ if color == Red then
                                    "red"
                                   else
                                    "black"
                        ]
                        []
                    , text_
                        [ SvgAttributes.transform <|
                            "translate(0,"
                                ++ (String.fromInt <| 30 - (2 * depth))
                                ++ ")"
                        , SvgAttributes.style <|
                            "font-size:"
                                ++ (String.fromInt <| 16 - depth)
                                ++ "px; font-family: sans-serif; text-anchor: middle;fill: black"
                        ]
                        [ text <| Debug.toString val ]
                    ]
                )
                    :: (viewTree left lx ny (depth + 1)
                            ++ viewTree right rx ny (depth + 1)
                       )


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ text <| "svg g { transition: transform 0.1s; }" ]
        , Svg.svg [ SvgAttributes.width "100%", SvgAttributes.viewBox "0 0 1200 800" ]
            [ Svg.Keyed.node "g" [] <| viewTree model.tree 600 30 0
            ]
        ]


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
