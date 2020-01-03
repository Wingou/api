module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, img, text, input, button, table, tr, td)
import Html.Attributes exposing (src, style, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, request, emptyBody, expectJson, header)
import Json.Decode exposing (Decoder, at, field, int, map8, string, map, list)
import String exposing (fromInt)
import List exposing (head)


-- MODEL


type alias Task =
    {
        id: Int,
        user_validator: String,
        status: String,
        processed_by: String,
        reprise_type: String,
        creation_date: String,
        modification_date: String,
        master_mode: String
    }

type alias Tasks = 
    List Task


type MasterMode =
        None
        | DAM 
        | NAS

type alias Model =
    { 
        operation : String,
        masterMode : MasterMode,
        tasks : List Task }



type Msg
    = NoOp
    | GotTasks (Result Http.Error (List Task))
    | CallGetApi
    | SetOperation String

emptyTask : Task
emptyTask = {
                id=-1,
                user_validator="",
                status="",
                processed_by="",
                reprise_type="",
                creation_date="",
                modification_date="",
                master_mode=""}

init : ( Model, Cmd Msg )
init =
    ( { 
        operation = "",
        masterMode = None,
        tasks =
            [ emptyTask
            ]
        }
    , Cmd.none
    )

urlAPI : String
urlAPI ="http://mediaapi.vpback.vpgrp.io/api/v1/tasks/" --PARAH9
-- urlAPI ="http://mediaapi-ci.vpback.vpgrp.io/api/v1/tasks/" --PARAH9
         -- url = "https://elm-lang.org/assets/public-opinion.txt"
         --  url = "https://ci-api-mediashare.vpback.vpgrp.io/api/context?page=1&size=1"
         -- url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
      

-- Functions 
requestGetTasks : String -> Cmd Msg
requestGetTasks operation =
    Http.request
        { 
              method = "GET"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = urlAPI ++ operation
            , body = emptyBody
            , expect = expectJson GotTasks listResultDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 

switchMasterMode : String -> MasterMode -> Cmd Msg
switchMasterMode operation antiMasterMode =
    Http.request
        { 
              method = "GET"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = urlAPI ++ operation
            , body = emptyBody
            , expect = expectJson GotTasks listResultDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 

-- helper
convertMasterModeToString : MasterMode -> String
convertMasterModeToString masterMode =
                    case masterMode of
                                None -> "None"
                                DAM -> "DAM"
                                NAS -> "NAS"

convertStringToMasterMode : String -> MasterMode
convertStringToMasterMode masterMode =
                    case masterMode of 
                                "DAM" -> DAM
                                "NAS" -> NAS
                                _ -> None

getAntiMasterMode : MasterMode -> MasterMode
getAntiMasterMode masterMode = 
           case masterMode of 
                                DAM -> NAS
                                NAS -> DAM
                                None -> None

-- Decoders 
resultDecoder : Decoder Task
resultDecoder =
    map8 Task
        (field "id" int)
        (field "user_validator" string)
        (field "status" string)
        (field "processed_by" string)
        (field "reprise_type" string)
        (field "creation_date" string)
        (field "modification_date" string)
        (field "master_mode" string)


listResultDecoder : Decoder (List Task)
listResultDecoder =
    map identity (list resultDecoder)
       

-- UPDATE



        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetOperation op ->
            ( {model | operation = op }, Cmd.none )


        CallGetApi ->
            ( model, requestGetTasks model.operation)

        GotTasks r ->
            let
                getTasks = 
                    case r of
                        Ok listGetTasks ->
                            listGetTasks
                        Err err ->
                            []
                getLastTask =
                        case head getTasks of
                            Just t ->
                                t

                            Nothing ->
                                emptyTask
            in
            ( { model
                | tasks =getTasks,
                  masterMode = convertStringToMasterMode getLastTask.master_mode
                     
              }
            , Cmd.none
            )



-- VIEW

displayTasks : Tasks -> Html Msg
displayTasks tasks =
              div []    
            (List.map (\t -> table [ style "border" "solid", style "width" "500px"][

                tr[][  td[][ text "Id"  ] , td[style "width" "250px"][ text (fromInt t.id)]  ],
                tr[][  td[][ text "user_validator"  ] , td[][ text t.user_validator]  ],
                tr[][  td[][ text "status"  ] , td[][ text t.status]  ],
                tr[][  td[][ text "processed_by"  ] , td[][ text t.processed_by]  ],
                tr[][  td[][ text "reprise_type"  ] , td[][ text t.reprise_type]  ],
                tr[][  td[][ text "creation_date"  ] , td[][ text t.creation_date]  ],
                tr[][  td[][ text "modification_date"  ] , td[][ text t.modification_date]  ],
                tr[][  td[][ text "master_mode"  ] , td[][ text t.master_mode]  ]
                    
            ]) tasks)



displayMasterMode : String -> MasterMode -> Html Msg
displayMasterMode operation masterMode =
    let
        displayMode=if masterMode==None then    
                "None"
            else
                "Block"
        
        masterModeDisplay=convertMasterModeToString masterMode
        antiMasterModeDisplay=convertMasterModeToString (getAntiMasterMode masterMode)
    in
    div[
        style "display" displayMode 
    ][ 
        text ("MASTER MODE : "++ masterModeDisplay) ,
        button [][ text ("Convert to "++antiMasterModeDisplay)]

    ]
    


view : Model -> Html Msg
view model =
    div []
        [
          div [][
                
                input [ onInput SetOperation, value model.operation, placeholder "Opération" ][], button [onClick CallGetApi ][text "OK"]
           ]
          ,
          displayMasterMode model.operation model.masterMode
          ,
          displayTasks model.tasks
          

      
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
