module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http exposing (get, request, emptyBody, expectJson, header)
import Json.Decode exposing (Decoder, at, field, int, map2, string, map, list)
import String exposing (fromInt)
import List


-- MODEL


type alias Meta =
    {
        user_validator : String,
        status : String
    }


type alias Model =
    { result : List Meta }


init : ( Model, Cmd Msg )
init =
    ( { result =
            [ 
                {user_validator = "INITIALISATION__1", status="INIT"},
                {user_validator = "INITIALISATION__2", status="INITiatlisation"}
            ]
        }
    , Cmd.none
    )

urlAPI : String
urlAPI ="http://mediaapi-ci.vpback.vpgrp.io/api/v1/tasks/PARAH9"
         -- url = "https://elm-lang.org/assets/public-opinion.txt"
         --  url = "https://ci-api-mediashare.vpback.vpgrp.io/api/context?page=1&size=1"
         -- url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
      

-- UPDATE


type Msg
    = NoOp
    | GotText (Result Http.Error (List Meta))
    | CallGetApi



getAPI : Cmd Msg
getAPI =
    Http.request
        { 
              method = "GET"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = urlAPI
            , body = emptyBody
            , expect = expectJson GotText resultDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 

uniqDecoder : Decoder Meta
uniqDecoder =
    map2 Meta
        (field "user_validator" string)
        (field "status" string) 


resultDecoder : Decoder (List Meta)
resultDecoder =
    map identity (list uniqDecoder)
       
        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CallGetApi ->
            ( model, getAPI )

        GotText r ->
            ( { model
                | result =
                    case r of
                        Ok s ->
                            s

                        Err err ->
                            [{ status = "ERROR"
                            , user_validator = "RIEN_ERROR"
                            }]
              }
            , Cmd.none
            )



-- VIEW

view : Model -> Html Msg
view model =
    div []
        [
          div []    
            (List.map (\m -> div [][
                    div [] [ text ("STATUS : " ++ m.status) ]
                    , div [] [ text ("EXTRRNAL_ID:  " ++ m.user_validator) ]
            ]) model.result)
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
