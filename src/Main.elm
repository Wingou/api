module Main exposing (Config, Environment(..), Model, Msg(..), Operation, ResponseApi, Task, Tasks, Workflow, Workflows, apiHeader, config, deleteTaskDecoder, displayButtonMasterMode, displayButtonMenu, displayEnv, displayHeader, displayInputOperation, displayMessageUser, displayOperation, displayStats, displayTasks, displayWorkflows, emptyOperation, emptyResponseApi, emptyTask, emptyWorkflow, fromBoolToColor, fromEnvToString, getLastTask, getLastWorkflow, init, kibanaUrl, main, operationDecoder, requestAbortTask, requestAbortWorkflow, requestDeleteTask, requestGetOperation, requestGetTasks, requestGetWorkflows, requestPatchMasterMode, requestPostIndexation, taskDecoder, tasksDecoder, toAntiMasterMode, track, update, version, view, workflowDecoder, workflowsDecoder)

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, button, div, hr, input, span, table, td, text, tr, br, h3)
import Html.Attributes exposing (attribute, href, placeholder, style, target, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Header, emptyBody, expectJson, expectWhatever, header, jsonBody, request)
import Json.Decode exposing (Decoder, at, bool, field, int, list, map, map3, map5, map8, string)
import Json.Encode as Encode
import List exposing (head, reverse, sortBy)
import String exposing (fromInt)
import Url exposing (Url, toString)



---------- CONSTANTES


version : String
version =
    "v0.1.5"

baseLine : String
baseLine = version  ++ " - Helpdesk Application by MediaProd - January 2020"

kibanaUrl : String
kibanaUrl =
    "https://kibana.noc.vpgrp.io/s/sourcing/app/kibana#/visualize/create?type=table&indexPattern=ba55b700-ffd4-11e9-926b-59f96530211d&_g=(filters:!(),refreshInterval:(pause:!t,value:0),time:(from:now-7d,mode:quick,to:now))&_a=(filters:!(('$state':(store:appState),meta:(alias:!n,disabled:!f,index:ba55b700-ffd4-11e9-926b-59f96530211d,key:app,negate:!f,params:(query:Pamela,type:phrase),type:phrase,value:Pamela),query:(match:(app:(query:Pamela,type:phrase)))),('$state':(store:appState),meta:(alias:!n,disabled:!f,index:ba55b700-ffd4-11e9-926b-59f96530211d,key:eventName,negate:!f,params:(query:click,type:phrase),type:phrase,value:click),query:(match:(eventName:(query:click,type:phrase))))),linked:!f,query:(language:lucene,query:''),uiState:(vis:(params:(sort:(columnIndex:0,direction:desc)))),vis:(aggs:!((enabled:!t,id:'1',params:(customLabel:Number),schema:metric,type:count),(enabled:!t,id:'2',params:(customInterval:'2h',drop_partials:!f,extended_bounds:(),field:reportTime,interval:h,min_doc_count:1,timeRange:(from:now-7d,mode:quick,to:now),useNormalizedEsInterval:!t),schema:bucket,type:date_histogram),(enabled:!t,id:'3',params:(field:data.vpaId-tags.keyword,missingBucket:!f,missingBucketLabel:Missing,order:desc,orderBy:'1',otherBucket:!f,otherBucketLabel:Other,size:5),schema:bucket,type:terms)),params:(perPage:10,showMetricsAtAllLevels:!f,showPartialRows:!f,showTotal:!f,sort:(columnIndex:!n,direction:!n),totalFunc:sum),title:'New%20Visualization',type:table))"


config : Environment -> Config
config selectedEnv =
    case selectedEnv of
        PROD ->
            { serverMediaAPI = "http://mediaapi.vpback.vpgrp.io/api/v1"
            , serverMSServerAPI = "https://api-mediashare3.vpback.vpgrp.io/"
            , defaultOperation = ""
            }

        PREPROD ->
            { serverMediaAPI = "http://mediaapi-pp.vpback.vpgrp.io/api/v1"
            , serverMSServerAPI = "https://pp-api-mediashare.vpback.vpgrp.io"
            , defaultOperation = ""
            }

        CI ->
            { serverMediaAPI = "http://mediaapi-ci.vpback.vpgrp.io/api/v1"
            , serverMSServerAPI = "https://ci-api-mediashare.vpback.vpgrp.io"
            , defaultOperation = "LADC5"
            }


apiHeader : List Header
apiHeader =
    [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg==" ]

dialogBoxOn : DialogBox
dialogBoxOn = {
                isDisplayed= True
                , bgColor = "lightgray"
                ,opacity = "0.83"
                , pointerEvent= "none" 
                }
dialogBoxOff : DialogBox
dialogBoxOff = 
                {
                isDisplayed= False
                , bgColor = "white"
                ,opacity = "1"
                , pointerEvent= "auto" 
                }

---------- TYPES


type Environment
    = CI
    | PREPROD
    | PROD


type Category
    = NOCATEGORY
    | TASKS
    | WORKFLOWS
    | MAGISTOR



---------- TYPES ALIAS


type alias Config =
    { serverMediaAPI : String
    , serverMSServerAPI : String
    , defaultOperation : String
    }


type alias Model =
    { op : Operation
    , operationInput : String
    , tasks : Tasks
    , responseApi : ResponseApi
    , workflows : Workflows
    , displayedCategory : Category
    , key : Nav.Key
    , url : Url
    , messageUser : String
    , env : Environment
    , dialogBox : DialogBox
    }


type alias Operation =
    { operationId : Int
    , operationCode : String
    , masterMode : String
    }


type alias Task =
    { id : Int
    , user_validator : String
    , status : String
    , processed_by : String
    , reprise_type : String
    , creation_date : String
    , modification_date : String
    , master_mode : String
    }


type alias Tasks =
    List Task


type alias ResponseApi =
    { success : Bool
    , status : Int
    , data : String
    }


type alias Workflow =
    { id : String
    , status : String
    , user : String
    , created : String
    , started : String
    }


type alias Workflows =
    List Workflow

type alias DialogBox =
    {
        isDisplayed : Bool
        , bgColor : String
        ,opacity : String
        , pointerEvent : String 
    }



---------- TYPE Msg


type Msg
    = NoOp
    | SetEnv Environment
    | SetOperationInput String
    | CallGetOperation
    | ResultOperation (Result Http.Error Operation)
    | CallGetTasks
    | ResultTasks (Result Http.Error (List Task))
    | CallSwitchMasterMode
    | ResultSwitchMasterMode String (Result Http.Error ())
    | CallDeleteTask Int
    | ResultDeleteTask (Result Http.Error ResponseApi)
    | CallGetWorkflows
    | ResultWorkflows (Result Http.Error Workflows)
    | CallAbortWorkflow String
    | ResultAbortWorkflow (Result Http.Error ())
    | CallAbortTask Int
    | ResultAbortTask (Result Http.Error ())
    | CallIndexation Operation String
    | ResultIndexation Bool (Result Http.Error ())
    | CallInitMagistor
    | ResultInitMagistor (Result Http.Error ())
    | CallGetMagistor
    | CallDialogBox String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url



---------- INITIALIZE


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { op = emptyOperation
      , operationInput = (config PROD).defaultOperation
      , tasks = [ emptyTask ]
      , responseApi = emptyResponseApi
      , workflows = [ emptyWorkflow ]
      , displayedCategory = NOCATEGORY
      , key = key
      , url = url
      , messageUser = ""
      , env = PROD
    , dialogBox = dialogBoxOff
      }
    , Cmd.none
    )


emptyOperation : Operation
emptyOperation =
    { operationId = -1
    , operationCode = "OPERATION0"
    , masterMode = "NONE"
    }


emptyTask : Task
emptyTask =
    { id = -1
    , user_validator = ""
    , status = ""
    , processed_by = ""
    , reprise_type = ""
    , creation_date = ""
    , modification_date = ""
    , master_mode = ""
    }


emptyResponseApi : ResponseApi
emptyResponseApi =
    { success = False
    , status = 0
    , data = "Empty"
    }


emptyWorkflow : Workflow
emptyWorkflow =
    { id = "-1"
    , status = "INIT"
    , user = "Nobody"
    , created = ""
    , started = ""
    }



---------- REQUESTS


requestGetTasks : String -> Environment -> Cmd Msg
requestGetTasks op currentEnv =
    Http.request
        { method = "GET"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/tasks/"
                ++ op
        , body = emptyBody
        , expect = expectJson ResultTasks tasksDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestGetOperation : String -> Environment -> Cmd Msg
requestGetOperation op currentEnv =
    Http.request
        { method = "GET"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/operations/"
                ++ op
        , body = emptyBody
        , expect = expectJson ResultOperation operationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestPostIndexation : Operation -> String -> Environment -> Cmd Msg
requestPostIndexation op masterMode currentEnv =
    let
        switchAndIndex =
            op.masterMode == masterMode
    in
    Http.request
        { method = "POST"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/operations/"
                ++ op.operationCode
                ++ "/index/VALID?masterMode="
                ++ masterMode
        , body = emptyBody
        , expect = expectWhatever (ResultIndexation switchAndIndex)
        , timeout = Nothing
        , tracker = Nothing
        }


requestPatchMasterMode : Operation -> Environment -> Cmd Msg
requestPatchMasterMode op currentEnv =
    let
        operationId =
            op.operationId

        antiMasterMode =
            toAntiMasterMode op.masterMode
    in
    Http.request
        { method = "PATCH"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/operations/"
                ++ fromInt operationId
        , body =
            jsonBody
                (Encode.object
                    [ ( "Op", Encode.string "UPDATE" )
                    , ( "Path", Encode.string "master_mode" )
                    , ( "Value", Encode.string antiMasterMode )
                    ]
                )
        , expect = expectWhatever (ResultSwitchMasterMode antiMasterMode)
        , timeout = Nothing
        , tracker = Nothing
        }


requestDeleteTask : Int -> Environment -> Cmd Msg
requestDeleteTask taskId currentEnv =
    Http.request
        { method = "DELETE"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/tasks/"
                ++ fromInt taskId
        , body = emptyBody
        , expect = expectJson ResultDeleteTask deleteTaskDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestAbortTask : Int -> Environment -> Cmd Msg
requestAbortTask taskId currentEnv =
    Http.request
        { method = "DELETE"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/tasks/"
                ++ fromInt taskId
        , body = emptyBody
        , expect = expectWhatever ResultAbortTask
        , timeout = Nothing
        , tracker = Nothing
        }


requestGetWorkflows : Operation -> Environment -> Cmd Msg
requestGetWorkflows op currentEnv =
    Http.request
        { method = "GET"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/operations/"
                ++ op.operationCode
                ++ "/workflows"
        , body = emptyBody
        , expect = expectJson ResultWorkflows workflowsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestAbortWorkflow : String -> Environment -> Cmd Msg
requestAbortWorkflow workflowId currentEnv =
    Http.request
        { method = "POST"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMediaAPI
                ++ "/workflows/"
                ++ workflowId
                ++ "/abort"
        , body = emptyBody
        , expect = expectWhatever ResultAbortWorkflow
        , timeout = Nothing
        , tracker = Nothing
        }


requestPostInitMagistor : Operation -> Environment -> Cmd Msg
requestPostInitMagistor op currentEnv =
    Http.request
        { method = "POST"
        , headers = apiHeader
        , url =
            (config currentEnv).serverMSServerAPI
                ++ "/api/context/"
                ++ op.operationCode
                ++ "/source-legacy-references"
        , body = emptyBody
        , expect = expectWhatever ResultInitMagistor
        , timeout = Nothing
        , tracker = Nothing
        }



---------- HELPERS


toAntiMasterMode : String -> String
toAntiMasterMode masterMode =
    case masterMode of
        "DAM" ->
            "NAS"

        "NAS" ->
            "DAM"

        _ ->
            "NONE"


getLastTask : Tasks -> Task
getLastTask t =
    case head t of
        Just justTask ->
            justTask

        Nothing ->
            emptyTask


getLastWorkflow : Workflows -> Workflow
getLastWorkflow w =
    case head w of
        Just justWorkflow ->
            justWorkflow

        Nothing ->
            emptyWorkflow


fromBoolToColor : Bool -> String
fromBoolToColor b =
    if b then
        "yellow"

    else
        "lightgray"


fromEnvToString : Environment -> String
fromEnvToString environnement =
    case environnement of
        PROD ->
            "PROD"

        PREPROD ->
            "PREPROD"

        CI ->
            "CI"



---------- DECODERS


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
    list taskDecoder


operationDecoder : Decoder Operation
operationDecoder =
    map3 Operation
        (field "id" int)
        (field "label" string)
        (field "master_mode" string)


deleteTaskDecoder : Decoder ResponseApi
deleteTaskDecoder =
    map3 ResponseApi
        (field "success" bool)
        (field "status" int)
        (field "data" string)


workflowsDecoder : Decoder Workflows
workflowsDecoder =
    at [ "Items" ] (list workflowDecoder)


workflowDecoder : Decoder Workflow
workflowDecoder =
    map5 Workflow
        (field "Id" string)
        (field "Status" string)
        (field "User" string)
        (field "Created" string)
        (field "Started" string)



---------- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        opInput =
            model.operationInput

        modelOp =
            model.op

        operationCode =
            modelOp.operationCode

        currentEnv =
            model.env
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ---------- SETS
        SetEnv selectedEnv ->
            ( { model
                | env = selectedEnv
                , op = emptyOperation
                , tasks = [ emptyTask ]
                , responseApi = emptyResponseApi
                , workflows = [ emptyWorkflow ]
                , displayedCategory = NOCATEGORY
                , messageUser = ""
              }
            , Cmd.none
            )

        SetOperationInput op ->
            ( { model | operationInput = op }
            , Cmd.none
            )

        ---------- CALLS
        CallGetTasks ->
            if model.displayedCategory == TASKS then
                ( { model
                    | displayedCategory = NOCATEGORY
                    , messageUser = ""
                  }
                , Cmd.none
                )

            else
                ( { model
                    | displayedCategory = TASKS
                    , messageUser = ""
                  }
                , requestGetTasks opInput currentEnv
                )

        CallGetOperation ->
            ( { model
                | tasks = [ emptyTask ]
                , workflows = [ emptyWorkflow ]
                , displayedCategory = NOCATEGORY
                , messageUser = ""
              }
            , requestGetOperation opInput currentEnv
            )

        CallIndexation op masterMode ->
            let
                operation =
                    { op | masterMode = masterMode }
            in
            ( { model
                | displayedCategory = TASKS
                , op = operation
              }
            , requestPostIndexation op masterMode currentEnv
            )

        CallSwitchMasterMode ->
            ( { model
                | displayedCategory = TASKS
              }
            , requestPatchMasterMode modelOp currentEnv
            )

        CallDeleteTask taskId ->
            ( model, requestDeleteTask taskId currentEnv )

        CallGetWorkflows ->
            if model.displayedCategory == WORKFLOWS then
                ( { model
                    | displayedCategory = NOCATEGORY
                    , messageUser = ""
                  }
                , Cmd.none
                )

            else
                ( { model
                    | displayedCategory = WORKFLOWS
                    , messageUser = ""
                  }
                , requestGetWorkflows modelOp currentEnv
                )

        CallAbortWorkflow workflowId ->
            ( model, requestAbortWorkflow workflowId currentEnv )

        CallAbortTask taskId ->
            ( model, requestAbortTask taskId currentEnv )

        CallInitMagistor ->
            ( { model
                | displayedCategory = MAGISTOR
                 , messageUser = "Initialisation en cours..."
                , dialogBox = dialogBoxOff
              }
            , requestPostInitMagistor modelOp currentEnv
            )

        CallGetMagistor ->
            if model.displayedCategory == MAGISTOR then
                ( { model
                    | displayedCategory = NOCATEGORY
                    , messageUser = ""
                  }
                , Cmd.none
                )

            else
                ( { model
                    | displayedCategory = MAGISTOR
                    , messageUser = ""
                  }
                , Cmd.none
                )

        CallDialogBox status ->
            let
                dialogBoxSwitch = if status=="ON" then
                                        dialogBoxOn
                                    else
                                        dialogBoxOff
            in
            ( { model
                | dialogBox = dialogBoxSwitch
              }
            , Cmd.none
            )

        ---------- RESULTS
        ResultTasks r ->
            let
                getTasks =
                    case r of
                        Ok listGetTasks ->
                            listGetTasks

                        Err _ ->
                            []
            in
            ( { model
                | tasks = getTasks
              }
            , Cmd.none
            )

        ResultOperation r ->
            let
                gotOperation =
                    case r of
                        Ok opeOk ->
                            opeOk

                        Err _ ->
                            emptyOperation
            in
            ( { model
                | op = gotOperation
              }
            , requestGetTasks opInput currentEnv
            )

        ResultDeleteTask r ->
            let
                gotResponseApi =
                    case r of
                        Ok apiOK ->
                            apiOK

                        Err _ ->
                            emptyResponseApi
            in
            ( { model
                | responseApi = gotResponseApi
              }
            , requestGetTasks opInput currentEnv
            )

        ResultWorkflows r ->
            let
                gotWorkflows =
                    case r of
                        Ok w ->
                            w

                        Err _ ->
                            [ emptyWorkflow ]
            in
            ( { model
                | workflows = gotWorkflows
              }
            , Cmd.none
            )

        ResultAbortWorkflow _ ->
            ( { model
                | messageUser =
                    "Le bouton publication de la vente "
                        ++ operationCode
                        ++ " est débloqué."
              }
            , requestGetWorkflows modelOp currentEnv
            )

        ResultSwitchMasterMode masterMode _ ->
            ( { model
                | displayedCategory = TASKS
                , messageUser =
                    "La vente "
                        ++ operationCode
                        ++ " est passée en mode "
                        ++ masterMode
                        ++ "."
              }
            , requestGetOperation opInput currentEnv
            )

        ResultAbortTask _ ->
            ( { model
                | messageUser = "La task en PENDING est supprimée."
              }
            , requestGetTasks opInput currentEnv
            )

        ResultIndexation switchAndIndex _ ->
            let
                msgIndexation =
                    if switchAndIndex then
                        "L'indexation "
                            ++ modelOp.masterMode
                            ++ " de la vente "
                            ++ operationCode
                            ++ " est en cours."

                    else
                        "La vente "
                            ++ operationCode
                            ++ " est passée en mode "
                            ++ modelOp.masterMode
                            ++ " et l'indexation est en cours."
            in
            ( { model
                | messageUser = msgIndexation
              }
            , requestGetTasks opInput currentEnv
            )

        ResultInitMagistor r ->
            let
                msgInitMagistor =
                    case r of
                        Ok _ ->
                            "La vente "
                                ++ operationCode
                                ++ " a été réinitialisée et resynchronisée avec Magistor."

                        Err e ->
                            let
                                errorMsg =
                                    case e of
                                        Http.BadUrl url ->
                                            "Bad URL : " ++ url

                                        Http.Timeout ->
                                            "Time out."

                                        Http.NetworkError ->
                                            "Network error."

                                        Http.BadStatus i ->
                                            fromInt i

                                        Http.BadBody body ->
                                            "Bad Body - " ++ body
                            in
                            "La vente "
                                ++ operationCode
                                ++ " n'a pas pu être réinitialisée. Error : "
                                ++ errorMsg
            in
            ( { model
                | messageUser = msgInitMagistor
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
              }
            , Cmd.none
            )



---------- DISPLAYS


displayFooter : String -> Html Msg
displayFooter v =
    let
        msgFooter =
            "Pamela "
                ++ v
                ++ " - Helpdesk Application by MediaProd - January 2020"
    in
    div []
        [ hr [] []
        , div
            [ attribute "align" "center"
            , style "font-family" "arial"
            , style "font-size" "12px"
            ]
            [ text msgFooter ]
        ]


displayWorkflows : Workflows -> Category -> String -> Environment -> Html Msg
displayWorkflows workflows category operationCode environment =
    let
        workflowsSortedByStarted =
            reverse (sortBy .started workflows)

        lastWorkflow =
            getLastWorkflow workflowsSortedByStarted

        displayMode =
            if category == WORKFLOWS then
                "block"

            else
                "none"

        messageNoWorkflow =
            if lastWorkflow.id == "-1" then
                "No workflow found"

            else
                ""
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , div [ style "color" "RED" ]
            [ text messageNoWorkflow ]
        , div []
            (List.map
                (\w ->
                    table [ style "border" "solid", style "width" "100%" ]
                        [ tr []
                            [ td [ style "width" "30%" ] [ text "PUBLICATION WORKFLOWS" ]
                            , td [ style "width" "70%" ]
                                [ if w.id == lastWorkflow.id && w.status /= "ABORTED" && w.status /= "ENDED" then
                                    button
                                        [ onClick (CallAbortWorkflow w.id)
                                        , track ("Abort publication on " ++ operationCode) environment
                                        , attribute "title" "Abort the current publication"
                                        , style "width" "200px"
                                        ]
                                        [ text "Abort this Publication" ]

                                  else
                                    text ""
                                ]
                            ]
                        , tr []
                            [ td [] [ text "Id" ]
                            , td [] [ text w.id ]
                            ]
                        , tr []
                            [ td [] [ text "User" ]
                            , td [] [ text w.user ]
                            ]
                        , tr []
                            [ td [] [ text "Status" ]
                            , td [ style "color" "blue" ] [ text (w.status ++ " ") ]
                            ]
                        , tr []
                            [ td [] [ text "Created" ]
                            , td [] [ text w.created ]
                            ]
                        , tr []
                            [ td [] [ text "Started" ]
                            , td [] [ text w.started ]
                            ]
                        ]
                )
                (List.filter (\wf -> wf.id /= "-1") workflowsSortedByStarted)
            )
        ]


displayTasks : Tasks -> Category -> String -> Environment -> Html Msg
displayTasks tasks display operationCode environnement =
    let
        displayMode =
            if display == TASKS then
                "block"

            else
                "none"

        messageNoTask =
            if List.length tasks == 0 then
                "No task found"

            else
                ""
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , div [ style "color" "RED" ] [ text messageNoTask ]
        , div []
            (List.map
                (\t ->
                    let
                        pendingColor =
                            if t.status == "Pending" then
                                "orange"

                            else
                                "black"
                    in
                    table
                        [ style "border" "solid"
                        , style "width" "100%"
                        ]
                        [ tr []
                            [ td [ style "width" "30%" ] [ text "TASKS" ]
                            , td [ style "width" "70%" ]
                                [ if t.status == "Pending" then
                                    button
                                        [ style "width" "200px"
                                        , onClick (CallAbortTask t.id)
                                        , track ("Delete Pending task on " ++ operationCode) environnement
                                        ]
                                        [ text "Delete this pending task" ]

                                  else
                                    text ""
                                ]
                            ]
                        , tr []
                            [ td [] [ text "Id" ]
                            , td [] [ text (fromInt t.id) ]
                            ]
                        , tr []
                            [ td [] [ text "master_mode" ]
                            , td [ style "color" "blue" ] [ text t.master_mode ]
                            ]
                        , tr []
                            [ td [] [ text "status" ]
                            , td [ style "color" pendingColor ] [ text t.status ]
                            ]
                        , tr []
                            [ td [] [ text "user_validator" ]
                            , td [] [ text t.user_validator ]
                            ]
                        , tr []
                            [ td [] [ text "processed_by" ]
                            , td [] [ text t.processed_by ]
                            ]
                        , tr []
                            [ td [] [ text "reprise_type" ]
                            , td [] [ text t.reprise_type ]
                            ]
                        , tr []
                            [ td [] [ text "creation_date" ]
                            , td [] [ text t.creation_date ]
                            ]
                        , tr []
                            [ td [] [ text "modification_date" ]
                            , td [] [ text t.modification_date ]
                            ]
                        ]
                )
                tasks
            )
        ]


displayOperation : Operation -> Html Msg
displayOperation o =
    let
        displayMode =
            if o.operationId == -1 then
                "none"

            else
                "block"
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , table [ style "border" "solid", style "width" "500px" ]
            [ tr []
                [ td [] [ text "OperationId" ]
                , td [ style "width" "250px" ] [ text (fromInt o.operationId) ]
                ]
            , tr []
                [ td [] [ text "OperationCode" ]
                , td [] [ text o.operationCode ]
                ]
            , tr []
                [ td [] [ text "Master Mode" ]
                , td [] [ text o.masterMode ]
                ]
            ]
        ]


displayButtonMasterMode : Operation -> Task -> Category -> Environment -> Html Msg
displayButtonMasterMode op lastTask category environnement =
    let
        displayMode =
            if op.masterMode == "NONE" then
                "none"

            else if category == TASKS then
                "block"

            else
                "none"

        masterModeStr =
            op.masterMode

        antiMasterModeStr =
            toAntiMasterMode op.masterMode

        statusOfLastTask =
            lastTask.status

        enableMasterModeSwitch =
            if statusOfLastTask == "Pending" then
                "disabled"

            else
                "enabled"

        buttonLabelMasterModeIndexation =
            "Switch "
                ++ masterModeStr
                ++ " to "
                ++ antiMasterModeStr
                ++ " + Indexation "
                ++ antiMasterModeStr

        buttonLabelMasterMode =
            "Switch "
                ++ masterModeStr
                ++ " to "
                ++ antiMasterModeStr

        msgTrack_SwitchMasterMode =
            "Switch MasterMode to "
                ++ antiMasterModeStr
                ++ " on "
                ++ op.operationCode

        msgTitle_MasterModeCannotBeChanged =
            "The Master Mode can not be changed because the STATUS of the last task is Pending..."

        msgTitle_SwitchMasterMode =
            "Switch the current Master Mode from "
                ++ masterModeStr
                ++ " to "
                ++ antiMasterModeStr

        msgTrack_Indexation =
            "Launch the indexation "
                ++ masterModeStr
                ++ " on "
                ++ op.operationCode

        msgTitle_IndexationNotAvailable =
            "The indexation is not available because a task has already been pending..."

        msgTitle_Indexation =
            "Launch an indexation "
                ++ masterModeStr

        msgButton_Indexation =
            "Indexation "
                ++ masterModeStr

        msgTitle_SwitchMasterMode_Indexation =
            "Switch the current Master Mode from "
                ++ masterModeStr
                ++ " to "
                ++ antiMasterModeStr
                ++ " + Indexation "
                ++ antiMasterModeStr
    in
    div
        [ style "display" displayMode ]
        [ hr [] []
        , button
            [ style "width" "200px"
            , attribute enableMasterModeSwitch ""
            , track msgTrack_SwitchMasterMode environnement
            , if enableMasterModeSwitch == "disabled" then
                attribute "title" msgTitle_MasterModeCannotBeChanged

              else
                attribute "title" msgTitle_SwitchMasterMode
            , onClick CallSwitchMasterMode
            ]
            [ text buttonLabelMasterMode ]
        , text " "
        , button
            [ style "width" "200px"
            , onClick (CallIndexation op masterModeStr)
            , attribute enableMasterModeSwitch ""
            , track msgTrack_Indexation environnement
            , if enableMasterModeSwitch == "disabled" then
                attribute "title" msgTitle_IndexationNotAvailable

              else
                attribute "title" msgTitle_Indexation
            ]
            [ text msgButton_Indexation ]
        , if masterModeStr == "NAS" then
            text ""

          else
            text " > "
        , if masterModeStr == "NAS" then
            text ""

          else
            button
                [ style "width" "400px"
                , attribute enableMasterModeSwitch ""
                , track msgTrack_SwitchMasterMode environnement
                , if enableMasterModeSwitch == "disabled" then
                    attribute "title" msgTitle_MasterModeCannotBeChanged

                  else
                    attribute "title" msgTitle_SwitchMasterMode_Indexation
                , onClick (CallIndexation op antiMasterModeStr)
                ]
                [ text buttonLabelMasterModeIndexation ]
        ]


displayButtonCategory : Operation -> Category -> Category -> Html Msg
displayButtonCategory op categoryButton categoryOn =
    let
        displayMode =
            if op.operationId == -1 then
                "none"

            else
                "block"

        isDisp =
            categoryButton == categoryOn

        msgButton_Tasks =
            "Tasks"

        msgButton_Workflows =
            "Publication Workflows"

        msgButton_Magistor =
            "Magistor"
    in
    div
        [ style "display" displayMode ]
        [ case categoryButton of
            TASKS ->
                button
                    [ onClick CallGetTasks
                    , style "width" "200px"
                    , style "background-color" (fromBoolToColor isDisp)
                    ]
                    [ text msgButton_Tasks ]

            WORKFLOWS ->
                button
                    [ onClick CallGetWorkflows
                    , style "width" "200px"
                    , style "background-color" (fromBoolToColor isDisp)
                    ]
                    [ text msgButton_Workflows ]

            MAGISTOR ->
                button
                    [ onClick CallGetMagistor
                    , style "width" "200px"
                    , style "background-color" (fromBoolToColor isDisp)
                    ]
                    [ text msgButton_Magistor ]

            NOCATEGORY ->
                text ""
        ]


displayButtonMagistor : Operation -> Category -> Environment -> Html Msg
displayButtonMagistor op category environnement =
    let
        displayMode =
            if category == MAGISTOR then
                "block"

            else
                "none"

        msgTrack_InitMagistor =
            "Magistor initialization on "
                ++ op.operationCode

        msgTitle_InitializeSale =
            "Initialize the sale "
                ++ op.operationCode
                ++ " with Magistor data"

        msgButton_initialize =
            "Initialize "
                ++ op.operationCode
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , button
            [ track msgTrack_InitMagistor environnement
            , onClick (CallDialogBox "ON")
            , attribute "title" msgTitle_InitializeSale
            , style "width" "200px"
            ]
            [ text msgButton_initialize ]
        ]


displayMessageUser : String -> Html Msg
displayMessageUser message =
    div []
        [ if message == "" then
            text message

          else
            div []
                [ hr [] []
                , text message
                ]
        ]


displayButtonMenu : Operation -> Category -> Html Msg
displayButtonMenu op category =
    let
        displayMode =
            if op.operationId == -1 then
                "none"

            else
                "block"

        msgTitle_Tasks =
            "Check the Tasks | Switch Master Mode"

        msgTitle_Workflows =
            "Check the Publication Workflows | Abort Publication"

        msgTitle_Magistors =
            "Reinitialize the sale with Magistor data"
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , div [ style "display" "flex" ]
            [ div
                [ style "flex" "1"
                , attribute "title" msgTitle_Tasks
                ]
                [ displayButtonCategory op TASKS category ]
            , div
                [ style "flex" "1"
                , attribute "title" msgTitle_Workflows
                ]
                [ displayButtonCategory op WORKFLOWS category ]
            , div
                [ style "flex" "1"
                , attribute "title" msgTitle_Magistors
                ]
                [ displayButtonCategory op MAGISTOR category ]
            ]
        ]


displayHeader : Environment -> String -> Html Msg
displayHeader selectedEnv baseLineVersion  =
    div [ style "display" "flex", style "flex-direction" "row" ]
        [ div [ style "flex" "1", style "white-space" "nowrap" ] [ displayEnv selectedEnv ]
        , div [ style "flex" "50", style "text-align" "center" ] [ 
            
            div [style "font-family" "arial"
            , style "font-size" "20px"
            , style "font-weight" "bold"][ text "PAMELA" ]
        , 
        div [
             style "font-family" "arial"
            , style "font-size" "12px"

        ][ text baseLineVersion]
        ]
        , div [ style "flex" "1", style "white-space" "nowrap" ] [ displayStats ]
        ]


displayEnv : Environment -> Html Msg
displayEnv selectedEnv =
    let
        nextEnv =
            case selectedEnv of
                PROD ->
                    CI

                CI ->
                    PREPROD

                PREPROD ->
                    PROD

        msgTitle_swtichTo =
            "Switch to "
                ++ fromEnvToString nextEnv
    in
    div []
        [ text "Environment : "
        , span
            [ onClick (SetEnv nextEnv)
            , style "cursor" "pointer"
            , attribute "title" msgTitle_swtichTo
            ]
            [ text (fromEnvToString selectedEnv) ]
        ]


displayStats : Html Msg
displayStats =
    let
        msgA_statisticsOnKibana =
            "Statistics on Kibana"
    in
    div []
        [ a
            [ target "_blank"
            , href kibanaUrl
            ]
            [ text msgA_statisticsOnKibana ]
        ]


displayInputOperation : String -> Html Msg
displayInputOperation opInput =
    div []
        [ hr [] []
        , input
            [ onInput SetOperationInput
            , value opInput
            , placeholder "OperationCode"
            ]
            []
        , button
            [ onClick CallGetOperation
            , style "width" "100px"
            ]
            [ text "OK" ]
        ]

displayDialog : Bool -> Html Msg
displayDialog isDisplayed =   
            let
                displayMode = if isDisplayed then
                                    "block"
                                else
                                    "none"
            in
               div [
                            style "display" displayMode 
                            ,style "position" "absolute"
                            , style "border" "solid"
                            , style "background-color" "white"
                            ,style "left" "50%"
                            ,style "top" "30%"
                            ,style "width" "360px"
                            ,style "height" "160px"
                            ,style "margin-left" "-180px"
                            , style "margin-top" "-80px"
                            , style "text-align" "center"
                            ][
                                    h3[][text "WARNING !!!"],
                                    
                                   text "Every existing associations will be lost !"
                                   , br [][]
                                  
                                   , text "Do you really want to complete the reinitalization ?"
                                   , br[][],br[][],
                                   div [ style "display" "flex", style "flex-direction" "row"][
                                   div[style "flex" "1"][ ], button [style "flex" "1", onClick CallInitMagistor ][text "YES"] , div[style "flex" "1"][ ],  button [style "flex" "1", onClick (CallDialogBox "OFF")][text "NO"] 
                                   , div[style "flex" "1"][ ]
                                    ]    
                                   
                            ]   



---------- TRACKING


track : String -> Environment -> Attribute msg
track label environment =
    attribute "data-vpa-id"
        (label
            ++ " - "
            ++ fromEnvToString environment
        )



---------- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        modelTasks =
            model.tasks

        modelWorkflows =
            model.workflows

        modelOp =
            model.op

        modelDialogBox =
            model.dialogBox
    in
    { title = "Pamela " ++ version
    , body =
        [ 
            div [ style "margin" "1%" ]
            [
            div [ 
               style "position" "absolute"
               , style "width" "98%"
               , style "background-color" modelDialogBox.bgColor 
              , style "opacity" modelDialogBox.opacity 
              , style "pointer-events" modelDialogBox.pointerEvent 
              ]
            [ displayHeader model.env baseLine
            , displayInputOperation model.operationInput
            , displayOperation modelOp
            , displayButtonMenu modelOp model.displayedCategory
            , displayButtonMasterMode modelOp (getLastTask modelTasks) model.displayedCategory model.env
            , displayButtonMagistor modelOp model.displayedCategory model.env
            , displayMessageUser model.messageUser
            , displayWorkflows modelWorkflows model.displayedCategory modelOp.operationCode model.env
            , displayTasks modelTasks model.displayedCategory modelOp.operationCode model.env
            , displayFooter version
            ],

                displayDialog modelDialogBox.isDisplayed
                                     
            ]
        ]
    }



-------------------- PROGRAM --------------------


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
