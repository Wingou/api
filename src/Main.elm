module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, img, text, input, button, table, tr, td)
import Html.Attributes exposing (src, style, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, request, emptyBody, expectJson, header)
import Json.Decode exposing (Decoder, at, field, int, map8, map3,  string, map, list)
import String exposing (fromInt)
import List exposing (head)

---------- CONST

env : String
env = "Prod"
-- env = "CI"

type alias Config =
    {
        server : String,
        defaultOperation : String
    }

serverCI: String
serverCI =
    "http://mediaapi-ci.vpback.vpgrp.io/api/v1"

serverProd: String
serverProd =
    "http://mediaapi.vpback.vpgrp.io/api/v1"

defaultOperationCI: String
defaultOperationCI = "LADC5"    

defaultOperationProd: String
defaultOperationProd = "GNORWAY38"    

config: Config
config  = case env of
                            "Prod" -> {
                                        server=serverProd,
                                        defaultOperation=defaultOperationProd
                                    }
                            _ -> {
                                        server=serverCI,
                                        defaultOperation=defaultOperationCI
                                    }

---------- MODEL

type alias Model =
    {   op : Operation, 
        operationInput : String,
        tasks : List Task 
    }

type alias Operation =
    {
        operationId:Int,
        operationCode:String,
        masterMode:String
    }

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
        NONE
        | DAM 
        | NAS



type Msg
    = NoOp
    | GotTasks (Result Http.Error (List Task))
    | CallGetTasks
    | SetOperationInput String
    | GotOperation (Result Http.Error Operation)

emptyOperation : Operation
emptyOperation = {
        operationId=-1,
        operationCode = "OPERATION0",
        masterMode="NONE"
    }

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
        op = emptyOperation,
        operationInput = config.defaultOperation,
        tasks =
            [ emptyTask
            ]
        }
    , Cmd.none
    )


---------- API 
apiGetTasks : String
apiGetTasks = config.server ++ "/tasks/"

apiGetOperation : String
apiGetOperation = config.server ++ "/operations/"

---------- REQUEST
requestGetTasks : String -> Cmd Msg
requestGetTasks op =
    Http.request
        { 
              method = "GET"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = apiGetTasks ++ op
            , body = emptyBody
            , expect = expectJson GotTasks tasksDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 

requestGetOperation : String -> Cmd Msg
requestGetOperation op =
    Http.request
        { 
              method = "GET"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = apiGetOperation ++ op
            , body = emptyBody
            , expect = expectJson GotOperation operationDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 

---------- helper   

toAntiMasterMode : String -> String
toAntiMasterMode masterMode = 
           case masterMode of 
                                "DAM" -> "NAS"
                                "NAS" -> "DAM"
                                _ -> "NONE"

getLastTask : Tasks -> Task
getLastTask t =   
                    case head t of
                            Just justTask ->
                                justTask

                            Nothing ->
                                emptyTask


---------- Decoders 
taskDecoder : Decoder Task
taskDecoder =
    map8 Task
        (field "id" int)
        (field "user_validator" string)
        (field "status" string)
        (field "processed_by" string)
        (field "reprise_type" string)
        (field "creation_date" string)
        (field "modification_date" string)
        (field "master_mode" string)


tasksDecoder : Decoder (List Task)
tasksDecoder =
    map identity (list taskDecoder)
       
operationDecoder : Decoder Operation
operationDecoder =
    map3 Operation
        (field "id" int)
        (field "label" string)
        (field "master_mode" string)

---------- UPDATE



        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetOperationInput op ->
            ( {model | operationInput = op }, Cmd.none )


        CallGetTasks ->
            ( model, requestGetTasks model.operationInput)

        GotTasks r ->
            let
                getTasks = 
                    case r of
                        Ok listGetTasks ->
                            listGetTasks
                        Err err ->
                            []
            in
            ( { model
                | tasks =getTasks
              }
            , requestGetOperation model.operationInput
            )
        
        GotOperation r ->
            let
                gotOperation=
                    case r of
                        Ok opeOk ->
                            opeOk
                        Err err ->
                            emptyOperation
            in
            ( { model
                | op=gotOperation
              }
            , Cmd.none
            )



---------- VIEW

displayTasks : Tasks -> Html Msg
displayTasks tasks =
        let 
            lastTask = getLastTask tasks
          
            displayMode=
                if lastTask.id == -1 then    
                    "none"
                else
                    "block"
         in
            div [style "display" displayMode]   
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


displayOperation : Operation -> Html Msg
displayOperation o =
    let 
          displayMode=if o.operationId == -1 then    
                "none"
            else
                "block"
    in
    div [style "display" displayMode]
        [table [ style "border" "solid", style "width" "500px"][

                tr[][  td[][ text "OperationId"  ] , td[style "width" "250px"][ text (fromInt o.operationId)]  ],
                tr[][  td[][ text "OperationCode"  ] , td[][ text o.operationCode]  ],
                tr[][  td[][ text "Master Mode"  ] , td[][ text o.masterMode]  ]
                ]
                    
         ]

displayMasterMode : Operation ->  Html Msg
displayMasterMode op =
    let
        displayMode=if op.masterMode=="NONE" then    
                "none"
            else
                "block"
        
        masterModeStr=op.masterMode
        antiMasterModeStr= toAntiMasterMode op.masterMode
    in
    div[
        style "display" displayMode 
    ][ 
        text ("MASTER MODE : "++ masterModeStr) ,
        button [][ text ("Convert to "++antiMasterModeStr)]

    ]
    


view : Model -> Html Msg
view model =
    let
        modelOp=model.op
        modelTasks=model.tasks
    in
    div []
        [
          div [][
                
                input [ onInput SetOperationInput, value model.operationInput, placeholder "Opération" ][], button [onClick CallGetTasks ][text "OK"]
           ]
        , displayOperation model.op
          , displayMasterMode model.op
          , displayTasks modelTasks
          

      
        ]



-------------------- PROGRAM --------------------


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
