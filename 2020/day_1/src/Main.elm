module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { file : Status File
    , content : String
    , answerOne : Maybe Answer
    , answerTwo : Maybe Answer
    }


type alias Answer =
    { values : List Int
    , product : Int
    }


type Status a
    = NotRequested
    | Requested
    | Loaded a


init : () -> ( Model, Cmd Msg )
init _ =
    ( { file = NotRequested, content = "", answerOne = Nothing, answerTwo = Nothing }, Cmd.none )


requestFile : Cmd Msg
requestFile =
    Select.file [ "text/plain" ] FileLoaded


parseFile : File -> Cmd Msg
parseFile file =
    Task.perform FileContentLoaded (File.toString file)



-- UPDATE


type Msg
    = FileRequested
    | FileLoaded File
    | FileContentLoaded String
    | GetAnswerOne String
    | GetAnswerTwo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileRequested ->
            ( { model | file = Requested, answerOne = Nothing, answerTwo = Nothing }, requestFile )

        FileLoaded file ->
            ( { model | file = Loaded file }, parseFile file )

        FileContentLoaded content ->
            ( { model | content = content }, Cmd.none )

        GetAnswerOne content ->
            let
                numbers =
                    content
                        |> String.split "\n"
                        |> List.map String.toInt
                        |> List.filterMap identity

                answer =
                    findOneFriend numbers 2020
            in
            ( { model | answerOne = answer }, Cmd.none )

        GetAnswerTwo content ->
            let
                numbers =
                    content
                        |> String.split "\n"
                        |> List.map String.toInt
                        |> List.filterMap identity

                answer =
                    findTwoFriends numbers 2020
            in
            ( { model | answerTwo = answer }, Cmd.none )


findOneFriend : List Int -> Int -> Maybe Answer
findOneFriend numbers target =
    case numbers of
        [] ->
            Nothing

        x :: [] ->
            Nothing

        x :: xs ->
            if List.member (target - x) xs then
                Just { values = [ x, target - x ], product = List.product [ x, target - x ] }

            else
                findOneFriend xs target


findTwoFriends : List Int -> Int -> Maybe Answer
findTwoFriends numbers target =
    case numbers of
        [] ->
            Nothing

        x :: [] ->
            Nothing

        x :: xs ->
            case findOneFriend xs (target - x) of
                Just answer ->
                    Just
                        { values = x :: answer.values
                        , product = List.product [ x, answer.product ]
                        }

                Nothing ->
                    findTwoFriends xs target



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (Debug.toString model) ]
        , button [ onClick FileRequested ]
            [ text "Upload file"
            ]
        , case model.file of
            Loaded f ->
                div []
                    [ renderFileContent model
                    , renderPartOne model
                    , renderPartTwo model
                    ]

            _ ->
                text ""
        ]


renderFileContent : Model -> Html Msg
renderFileContent { file, content } =
    case file of
        Loaded f ->
            p [] [ text "Input:", div [] [ text content ] ]

        _ ->
            text ""


renderPartOne : Model -> Html Msg
renderPartOne { file, content, answerOne } =
    case file of
        Loaded f ->
            div []
                [ h2 [] [ text "Part 1." ]
                , p [] [ text "Find the two entries that sum to 2020; what do you get if you multiply them together?" ]
                , button [ onClick (GetAnswerOne content) ] [ text "Get answer" ]
                , Maybe.map renderAnswer answerOne |> Maybe.withDefault (text "")
                ]

        _ ->
            text ""


renderPartTwo : Model -> Html Msg
renderPartTwo { file, content, answerTwo } =
    case file of
        Loaded f ->
            div []
                [ h2 [] [ text "Part 2." ]
                , p [] [ text "In your expense report, what is the product of the three entries that sum to 2020?" ]
                , button [ onClick (GetAnswerTwo content) ] [ text "Get answer" ]
                , Maybe.map renderAnswer answerTwo |> Maybe.withDefault (text "")
                ]

        _ ->
            text ""


renderAnswer : Answer -> Html Msg
renderAnswer answer =
    text (Debug.toString answer)


fileDecoder : D.Decoder File
fileDecoder =
    D.at [ "target", "file" ] File.decoder
