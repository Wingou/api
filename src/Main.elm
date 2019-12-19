module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MODEL


type alias Model =
    { result : String }


init : ( Model, Cmd Msg )
init =
    ( { result = "RIEN" }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | GotText (Result Http.Error String)
    | CallGetApi


getPublicOpinion : Cmd Msg
getPublicOpinion =
    Http.get
        { 
            -- url = "https://elm-lang.org/assets/public-opinion.txt"
              --url = "https://ci-api-mediashare.vpback.vpgrp.io/api/context?page=1&size=1"
             url="https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
            , expect = Http.expectJson GotText resultDecoder
        }


resultDecoder : Decoder String
resultDecoder = 
    field "data" (field "image_url" string)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CallGetApi ->
            ( model, getPublicOpinion )

        GotText r ->
            ( {model| result = 
                    case r of
                        Ok s -> s
                        Err err -> "no"
                    }  , Cmd.none )



-- VIEW


apiGet : String
apiGet =
    "https://jsonplaceholder.typicode.com/users"


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.result ]
        , img [ src "images/door.jpg" ] []
        , a [ style "cursor" "pointer", onClick CallGetApi ] [ text "Call API" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
