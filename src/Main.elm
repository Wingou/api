module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, at, field, int, map3, string)
import String exposing (fromInt)



-- MODEL


type alias Meta =
    { status : Int
    , msg : String
    , response_id : String
    }


type alias Model =
    { result : Meta }


init : ( Model, Cmd Msg )
init =
    ( { result =
            { status = 0
            , msg = "RIEN"
            , response_id = "RIENNULL"
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | GotText (Result Http.Error Meta)
    | CallGetApi


getPublicOpinion : Cmd Msg
getPublicOpinion =
    Http.get
        { -- url = "https://elm-lang.org/assets/public-opinion.txt"
          --url = "https://ci-api-mediashare.vpback.vpgrp.io/api/context?page=1&size=1"
          url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
        , expect = Http.expectJson GotText resultDecoder
        }


resultDecoder : Decoder Meta
resultDecoder =
    map3 Meta
        (at [ "meta", "status" ] int)
        (at [ "meta", "msg" ] string)
        (at [ "meta", "response_id" ] string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CallGetApi ->
            ( model, getPublicOpinion )

        GotText r ->
            ( { model
                | result =
                    case r of
                        Ok s ->
                            s

                        Err err ->
                            { status = 0
                            , msg = "RIEN_ERROR"
                            , response_id = "RIENNULL_ERROR"
                            }
              }
            , Cmd.none
            )



-- VIEW


apiGet : String
apiGet =
    "https://jsonplaceholder.typicode.com/users"


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("STATUS : " ++ fromInt model.result.status) ]
        , div [] [ text ("MSG :  " ++ model.result.msg) ]
        , div [] [ text ("RESPONSE_ID :  " ++ model.result.response_id) ]
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
