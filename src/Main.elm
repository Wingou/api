module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, img, text, input, button, table, tr, td, hr)
import Html.Attributes exposing (src, style, value, placeholder, attribute)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, request, emptyBody, expectJson, header)
import Json.Decode exposing (Decoder, at, field, int, map8, map3,  string, map, list)
import String exposing (fromInt)
import List exposing (head)
import Http exposing (jsonBody)
import Json.Encode as Encode
---------- CONST

env : String
--env = "Prod"
env = "CI"

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

type Msg
    = NoOp
    | GotTasks (Result Http.Error (List Task))
    | CallGetTasks
    | SetOperationInput String
    | CallGetOperation
    | GotOperation (Result Http.Error Operation)
    | CallSwitchDAMtoNAS
    | CallSwitchNAStoDAM

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

---------- REQUEST
requestGetTasks : String -> Cmd Msg
requestGetTasks op =
    Http.request
        { 
              method = "GET"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = config.server ++ "/tasks/" ++ op
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
            , url = config.server ++ "/operations/" ++ op
            , body = emptyBody
            , expect = expectJson GotOperation operationDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 


--  ----------- DAM TO NAS --> réindexe
--   return post(
--     `${MEDIA_API_URI}/operations/${opCode}/index/VALID?masterMode=${saleMode}`,
--     {},
--     { headers }
--   ) 
requestPostDAMtoNAS : Operation -> Cmd Msg
requestPostDAMtoNAS op =
    let
        operation=op.operationCode
        antiMasterMode= toAntiMasterMode op.masterMode
    in
    Http.request
        { 
              method = "POST"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg=="]
            , url = config.server ++ "/operations/" ++ operation ++ "/index/VALID?masterMode="++ antiMasterMode
            , body = emptyBody
            , expect = expectJson GotOperation operationDecoder
            , timeout = Nothing
            , tracker = Nothing
        } 



-- ----------- NAS to DAM --> change mode
--   patch(
--     `${MEDIA_API_URI}/operations/${resp.id}`,
--     JSON.stringify({ Op: 'UPDATE', Path: 'master_mode', Value: saleMode }),
--     {
--       headers: createAuthHeaders({ 'content-type': 'application/json' })
--     }
requestPatchNAStoDAM : Operation -> Cmd Msg
requestPatchNAStoDAM op =
    let
        operationId=op.operationId
        antiMasterMode= toAntiMasterMode op.masterMode
    in
    Http.request
        { 
              method = "PATCH"
            , headers = [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg==" ]
            , url = config.server ++ "/operations/" ++ (fromInt operationId)
            , body = jsonBody (Encode.object [ 
                                         ("Op", Encode.string "UPDATE" )
                                        ,("Path", Encode.string "master_mode" )
                                        ,("Value", Encode.string antiMasterMode )
                                     ] )
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
    let
        opInput = model.operationInput
        modelOp = model.op
 
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetOperationInput op ->
            ( {model | operationInput = op }, Cmd.none )


        CallGetTasks ->
            ( model, requestGetTasks opInput)

        CallGetOperation ->
            ( {model | tasks = [emptyTask]}, requestGetOperation opInput)

        CallSwitchDAMtoNAS -> 
            ( model, requestPostDAMtoNAS modelOp)

        CallSwitchNAStoDAM ->
            ( model, requestPatchNAStoDAM modelOp)



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
            , Cmd.none
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
            , requestGetTasks opInput
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

displayCallMasterMode : Operation -> Task ->  Html Msg
displayCallMasterMode op lastTask =
    let
        displayMode=if op.masterMode=="NONE" then    
                "none"
            else
                "block"
        
        masterModeStr=op.masterMode
        antiMasterModeStr= toAntiMasterMode op.masterMode

        statusOfLastTask = lastTask.status

        enableMasterModeSwitch =
            if statusOfLastTask=="Pending" then
                "disabled"
            else
                "enabled"
    in
    div[
        style "display" displayMode 
    ][ 
        text ("MASTER MODE : ") ,
        button [ attribute enableMasterModeSwitch "",
                 
                    if enableMasterModeSwitch=="disabled" then
                       attribute "title"  "The Master Mode can not be changed because the STATUS of the last task is Pending..."
                    else 
                       attribute "title"  ""
                    ,
            if antiMasterModeStr=="DAM" then
                onClick CallSwitchNAStoDAM
            else
                onClick CallSwitchDAMtoNAS
            
             ][ text (masterModeStr++" to "++antiMasterModeStr)]
        
    ]
    
displayCallTasks : Operation ->  Html Msg
displayCallTasks op =
    let
        displayMode=if op.operationId == -1 then    
                "none"
            else
                "block"
    in
    div[
        style "display" displayMode 
    ][ 
        text ("TASKS : ") ,
        button [ onClick CallGetTasks ][ text ("Voir les Tasks de "++ op.operationCode)]

    ]

view : Model -> Html Msg
view model =
    let
        modelOp=model.op
        modelTasks=model.tasks
    in
    div [style "margin" "20px"]
        [
          div [][
                
                input [ onInput SetOperationInput, value model.operationInput, placeholder "Opération" ][]
                , button [onClick CallGetOperation ][text "OK"]
           ]
        , displayOperation model.op
          , displayCallMasterMode model.op (getLastTask modelTasks)
         -- , displayCallTasks model.op
         , hr[][]
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
