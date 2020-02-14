module Main exposing (Config, Env(..), Model, Msg(..), Operation, ResponseApi, Task, Tasks, Workflow, Workflows, apiHeader, config, deleteTaskDecoder, displayCallGetWorkflows, displayCallMasterMode, displayCallTasks, displayFooter, displayOperation, displayTasks, displayWorkflows, emptyOperation, emptyResponseApi, emptyTask, emptyWorkflow, env, fromBoolToString, fromEnvToString, getLastTask, getLastWorkflow, init, main, operationDecoder, requestAbortTask, requestAbortWorkflow, requestDeleteTask, requestGetOperation, requestGetTasks, requestGetWorkflows, requestPatchNAStoDAM, requestPostDAMtoNAS, taskDecoder, tasksDecoder, toAntiMasterMode, track, update, view, workflowDecoder, workflowsDecoder)

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, br, button, div, hr, input, table, td, text, tr)
import Html.Attributes exposing (attribute, href, placeholder, style, target, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Header, emptyBody, expectJson, expectWhatever, header, jsonBody, request)
import Json.Decode exposing (Decoder, at, bool, field, int, list, map, map3, map5, map8, string)
import Json.Encode as Encode
import List exposing (head, reverse, sortBy)
import String exposing (fromInt)
import Url exposing (Url, toString)



---------- CONFIG


type Env
    = CI
    | PREPROD
    | PROD


env : Env
env =
    PROD


version : String
version =
    "v0.1.1"


kibanaUrl : String
kibanaUrl =
    "https://kibana-test.noc.vpgrp.io/s/sourcing/app/kibana#/visualize/edit/fa0d9e70-4db4-11ea-a724-d5d66a9f9181?_g=(filters:!(),refreshInterval:(pause:!t,value:0),time:(from:now%2Fd,to:now%2Fd))&_a=(filters:!(('$state':(store:appState),meta:(alias:!n,disabled:!f,index:'7a38f240-001f-11ea-a263-6101354a1020',key:app,negate:!f,params:(query:Pamela),type:phrase,value:Pamela),query:(match:(app:(query:Pamela,type:phrase)))),('$state':(store:appState),meta:(alias:!n,disabled:!f,index:'7a38f240-001f-11ea-a263-6101354a1020',key:eventName,negate:!f,params:(query:click),type:phrase,value:click),query:(match:(eventName:(query:click,type:phrase))))),linked:!f,query:(language:kuery,query:''),uiState:(vis:(params:(sort:(columnIndex:1,direction:desc)))),vis:(aggs:!((enabled:!t,id:'1',params:(),schema:metric,type:count),(enabled:!t,id:'2',params:(field:attributes.value.keyword,missingBucket:!f,missingBucketLabel:Missing,order:desc,orderBy:'1',otherBucket:!f,otherBucketLabel:Other,size:20),schema:bucket,type:terms),(enabled:!t,id:'3',params:(drop_partials:!f,extended_bounds:(),field:reportTime,interval:auto,min_doc_count:1,timeRange:(from:now%2Fw,to:now%2Fw),useNormalizedEsInterval:!t),schema:bucket,type:date_histogram)),params:(dimensions:(buckets:!((accessor:0,aggType:terms,format:(id:terms,params:(id:string,missingBucketLabel:Missing,otherBucketLabel:Other)),params:()),(accessor:1,aggType:date_histogram,format:(id:date,params:(pattern:'YYYY-MM-DD%20HH:mm')),params:())),metrics:!((accessor:2,aggType:count,format:(id:number),params:()))),perPage:10,percentageCol:Count,showMetricsAtAllLevels:!f,showPartialRows:!f,showTotal:!t,sort:(columnIndex:!n,direction:!n),totalFunc:sum),title:Pamela,type:table))"


type alias Config =
    { server : String
    , defaultOperation : String
    }


config : Config
config =
    case env of
        PROD ->
            { server = "http://mediaapi.vpback.vpgrp.io/api/v1"
            , defaultOperation = "" -- GNORWAY38 -- AMARTINI2
            }

        PREPROD ->
            { server = "http://mediaapi-pp.vpback.vpgrp.io/api/v1"
            , defaultOperation = "" -- NSCASHMERE38 -- LADC5
            }

        CI ->
            { server = "http://mediaapi-ci.vpback.vpgrp.io/api/v1"
            , defaultOperation = ""
            }



---------- TYPES


type alias Model =
    { op : Operation
    , operationInput : String
    , tasks : Tasks
    , responseApi : ResponseApi
    , workflows : Workflows
    , displayWorkflows : Bool
    , displayTasks : Bool
    , key : Nav.Key
    , url : Url
    , messageUser : String
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
    , --  aborted: String,
      --  ended: String,
      created : String
    , started : String
    }


type alias Workflows =
    List Workflow



---------- TYPE Msg


type Msg
    = NoOp
    | SetOperationInput String
    | CallGetOperation
    | GotOperation (Result Http.Error Operation)
    | CallGetTasks
    | GotTasks (Result Http.Error (List Task))
    | CallSwitchDAMtoNAS
    | CallSwitchNAStoDAM
    | CallDeleteTask Int
    | GotDeleteTask (Result Http.Error ResponseApi)
    | CallGetWorkflows
    | GotWorkflows (Result Http.Error Workflows)
    | CallAbortWorkflow String
    | GotAbortWorkflow (Result Http.Error ())
    | GotSwitchDAMtoNAS (Result Http.Error ())
    | GotSwithcNAStoDAM (Result Http.Error ())
    | CallAbortTask Int
    | GotAbortTask (Result Http.Error ())
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotIndexation (Result Http.Error ())
    | CallIndexation Operation



---------- INITIALIZE


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { op = emptyOperation
      , operationInput = config.defaultOperation
      , tasks = [ emptyTask ]
      , responseApi = emptyResponseApi
      , workflows = [ emptyWorkflow ]
      , displayTasks = False
      , displayWorkflows = False
      , key = key
      , url = url
      , messageUser = ""
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



---------- REQUEST


apiHeader : List Header
apiHeader =
    [ header "Authorization" "Basic c3ZjX21lZGlhdGFza3NAb3JlZGlzLXZwLmxvY2FsOnBXTlpPJzkuWFJ3Rg==" ]


requestGetTasks : String -> Cmd Msg
requestGetTasks op =
    Http.request
        { method = "GET"
        , headers = apiHeader
        , url = config.server ++ "/tasks/" ++ op
        , body = emptyBody
        , expect = expectJson GotTasks tasksDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestGetOperation : String -> Cmd Msg
requestGetOperation op =
    Http.request
        { method = "GET"
        , headers = apiHeader
        , url = config.server ++ "/operations/" ++ op
        , body = emptyBody
        , expect = expectJson GotOperation operationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestPostIndexation : String -> String -> Cmd Msg
requestPostIndexation operationCode masterMode =
    Http.request
        { method = "POST"
        , headers = apiHeader
        , url = config.server ++ "/operations/" ++ operationCode ++ "/index/VALID?masterMode=" ++ masterMode
        , body = emptyBody
        , expect = expectWhatever GotIndexation
        , timeout = Nothing
        , tracker = Nothing
        }


requestPostDAMtoNAS : Operation -> Cmd Msg
requestPostDAMtoNAS op =
    let
        operation =
            op.operationCode

        antiMasterMode =
            toAntiMasterMode op.masterMode
    in
    Http.request
        { method = "POST"
        , headers = apiHeader
        , url = config.server ++ "/operations/" ++ operation ++ "/index/VALID?masterMode=" ++ antiMasterMode
        , body = emptyBody
        , expect = expectWhatever GotSwitchDAMtoNAS
        , timeout = Nothing
        , tracker = Nothing
        }


requestPatchNAStoDAM : Operation -> Cmd Msg
requestPatchNAStoDAM op =
    let
        operationId =
            op.operationId

        antiMasterMode =
            toAntiMasterMode op.masterMode
    in
    Http.request
        { method = "PATCH"
        , headers = apiHeader
        , url = config.server ++ "/operations/" ++ fromInt operationId
        , body =
            jsonBody
                (Encode.object
                    [ ( "Op", Encode.string "UPDATE" )
                    , ( "Path", Encode.string "master_mode" )
                    , ( "Value", Encode.string antiMasterMode )
                    ]
                )
        , expect = expectWhatever GotSwithcNAStoDAM
        , timeout = Nothing
        , tracker = Nothing
        }


requestDeleteTask : Int -> Cmd Msg
requestDeleteTask taskId =
    Http.request
        { method = "DELETE"
        , headers = apiHeader
        , url = config.server ++ "/tasks/" ++ fromInt taskId
        , body = emptyBody
        , expect = expectJson GotDeleteTask deleteTaskDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestAbortTask : Int -> Cmd Msg
requestAbortTask taskId =
    Http.request
        { method = "DELETE"
        , headers = apiHeader
        , url = config.server ++ "/tasks/" ++ fromInt taskId
        , body =
            emptyBody

        -- jsonBody
        --     (Encode.object
        --         [ ( "Op", Encode.string "UPDATE" )
        --         , ( "Path", Encode.string "status" )
        --         , ( "Value", Encode.string "Aborted" )
        --         ]
        --     )
        , expect = expectWhatever GotAbortTask
        , timeout = Nothing
        , tracker = Nothing
        }


requestGetWorkflows : Operation -> Cmd Msg
requestGetWorkflows op =
    Http.request
        { method = "GET"
        , headers = apiHeader
        , url = config.server ++ "/operations/" ++ op.operationCode ++ "/workflows"
        , body = emptyBody
        , expect = expectJson GotWorkflows workflowsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestAbortWorkflow : String -> Cmd Msg
requestAbortWorkflow workflowId =
    Http.request
        { method = "POST"
        , headers = apiHeader
        , url = config.server ++ "/workflows/" ++ workflowId ++ "/abort"
        , body = emptyBody
        , expect = expectWhatever GotAbortWorkflow
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


fromBoolToString : Bool -> String
fromBoolToString b =
    if b then
        "[-]"

    else
        "[+]"


fromEnvToString : Env -> String
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
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetOperationInput op ->
            ( { model | operationInput = op }, Cmd.none )

        CallGetTasks ->
            if model.displayTasks then
                ( { model | displayTasks = False, messageUser = "" }, Cmd.none )

            else
                ( { model | displayTasks = True, displayWorkflows = False, messageUser = "" }, requestGetTasks opInput )

        CallGetOperation ->
            ( { model
                | tasks = [ emptyTask ]
                , workflows = [ emptyWorkflow ]
                , displayWorkflows = False
                , displayTasks = False
                , messageUser = ""
              }
            , requestGetOperation opInput
            )

        CallSwitchDAMtoNAS ->
            ( { model
                | displayWorkflows = False
                , displayTasks = True
              }
            , requestPostDAMtoNAS modelOp
            )

        CallSwitchNAStoDAM ->
            ( { model
                | displayWorkflows = False
                , displayTasks = True
              }
            , requestPatchNAStoDAM modelOp
            )

        CallDeleteTask taskId ->
            ( model, requestDeleteTask taskId )

        CallGetWorkflows ->
            if model.displayWorkflows then
                ( { model
                    | displayWorkflows = False
                    , messageUser = ""
                  }
                , Cmd.none
                )

            else
                ( { model
                    | displayWorkflows = True
                    , displayTasks = False
                    , messageUser = ""
                  }
                , requestGetWorkflows modelOp
                )

        CallAbortWorkflow workflowId ->
            ( model, requestAbortWorkflow workflowId )

        CallAbortTask taskId ->
            ( model, requestAbortTask taskId )

        CallIndexation op ->
            ( model, requestPostIndexation op.operationCode op.masterMode )

        GotTasks r ->
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

        GotOperation r ->
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
            , requestGetTasks opInput
            )

        GotDeleteTask r ->
            let
                gotResponseApi =
                    case r of
                        Ok apiOK ->
                            apiOK

                        Err _ ->
                            emptyResponseApi
            in
            ( { model | responseApi = gotResponseApi }
            , requestGetTasks opInput
            )

        GotWorkflows r ->
            let
                gotWorkflows =
                    case r of
                        Ok w ->
                            w

                        Err _ ->
                            [ emptyWorkflow ]
            in
            ( { model | workflows = gotWorkflows }, Cmd.none )

        GotAbortWorkflow _ ->
            ( { model | messageUser = "Le bouton publication de la vente " ++ operationCode ++ " est débloqué." }, requestGetWorkflows modelOp )

        GotSwithcNAStoDAM _ ->
            ( { model | displayTasks = True, messageUser = "La vente " ++ operationCode ++ " est passée en mode DAM." }, requestGetOperation opInput )

        GotSwitchDAMtoNAS _ ->
            ( { model | messageUser = "La vente " ++ operationCode ++ " est passée en mode NAS + indexation en cours." }, requestGetTasks opInput )

        GotAbortTask _ ->
            ( { model | messageUser = "La task en PENDING est supprimée." }, requestGetTasks opInput )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        GotIndexation _ ->
            ( { model | messageUser = "Indexation de la vente " ++ operationCode ++ " en cours." }, requestGetTasks opInput )



---------- DISPLAYS


displayFooter : String -> Html Msg
displayFooter v =
    div []
        [ hr [] []
        , div
            [ attribute "align" "center"
            , style "font-family" "arial"
            , style "font-size" "12px"
            ]
            [ text ("Pamela " ++ v ++ " - Helpdesk Application by MediaProd - January 2020") ]
        ]


displayWorkflows : Workflows -> Bool -> String -> Html Msg
displayWorkflows workflows display operationCode =
    let
        workflowsSortedByStarted =
            reverse (sortBy .started workflows)

        lastWorkflow =
            getLastWorkflow workflowsSortedByStarted

        displayMode =
            if display then
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
        , div [ style "color" "RED" ] [ text messageNoWorkflow ]
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
                                        , track ("Abort publication on " ++ operationCode)
                                        ]
                                        [ text "Abort this Publication" ]

                                  else
                                    text ""
                                ]
                            ]
                        , tr [] [ td [] [ text "Id" ], td [] [ text w.id ] ]
                        , tr [] [ td [] [ text "User" ], td [] [ text w.user ] ]
                        , tr [] [ td [] [ text "Status" ], td [ style "color" "blue" ] [ text (w.status ++ " ") ] ]
                        , tr [] [ td [] [ text "Created" ], td [] [ text w.created ] ]
                        , tr [] [ td [] [ text "Started" ], td [] [ text w.started ] ]
                        ]
                )
                (List.filter (\wf -> wf.id /= "-1") workflowsSortedByStarted)
            )
        ]


displayTasks : Tasks -> Bool -> String -> Html Msg
displayTasks tasks display operationCode =
    let
        displayMode =
            if display then
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
                    table [ style "border" "solid", style "width" "100%" ]
                        [ tr []
                            [ td [ style "width" "30%" ] [ text "TASKS" ]
                            , td [ style "width" "70%" ]
                                [ if t.status == "Pending" then
                                    button [ onClick (CallAbortTask t.id), track ("Delete Pending task on " ++ operationCode) ] [ text "Delete this pending task" ]

                                  else
                                    text ""
                                ]
                            ]
                        , tr [] [ td [] [ text "Id" ], td [] [ text (fromInt t.id) ] ]
                        , tr [] [ td [] [ text "master_mode" ], td [ style "color" "blue" ] [ text t.master_mode ] ]
                        , tr [] [ td [] [ text "status" ], td [ style "color" pendingColor ] [ text t.status ] ]
                        , tr [] [ td [] [ text "user_validator" ], td [] [ text t.user_validator ] ]
                        , tr [] [ td [] [ text "processed_by" ], td [] [ text t.processed_by ] ]
                        , tr [] [ td [] [ text "reprise_type" ], td [] [ text t.reprise_type ] ]
                        , tr [] [ td [] [ text "creation_date" ], td [] [ text t.creation_date ] ]
                        , tr [] [ td [] [ text "modification_date" ], td [] [ text t.modification_date ] ]
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
            [ tr [] [ td [] [ text "OperationId" ], td [ style "width" "250px" ] [ text (fromInt o.operationId) ] ]
            , tr [] [ td [] [ text "OperationCode" ], td [] [ text o.operationCode ] ]
            , tr [] [ td [] [ text "Master Mode" ], td [] [ text o.masterMode ] ]
            ]
        ]


displayCallMasterMode : Operation -> Task -> Bool -> Html Msg
displayCallMasterMode op lastTask display =
    let
        displayMode =
            if op.masterMode == "NONE" then
                "none"

            else if display == True then
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

        buttonLabel =
            "Switch "
                ++ masterModeStr
                ++ " to "
                ++ antiMasterModeStr
                ++ (if antiMasterModeStr == "NAS" then
                        " + Indexation NAS"

                    else
                        ""
                   )
    in
    div
        [ style "display" displayMode ]
        [ hr [] []
        , button
            [ attribute enableMasterModeSwitch ""
            , track ("Switch MasterMode to " ++ antiMasterModeStr ++ " on " ++ op.operationCode)
            , if enableMasterModeSwitch == "disabled" then
                attribute "title" "The Master Mode can not be changed because the STATUS of the last task is Pending..."

              else
                attribute "title" ("Switch the current Master Mode " ++ masterModeStr ++ " to " ++ antiMasterModeStr)
            , if antiMasterModeStr == "DAM" then
                onClick CallSwitchNAStoDAM

              else
                onClick CallSwitchDAMtoNAS
            ]
            [ text buttonLabel ]
        , text " "
        , button
            [ onClick (CallIndexation op)
            , attribute enableMasterModeSwitch ""
            , if enableMasterModeSwitch == "disabled" then
                attribute "title" "The indexation is not available because a task has already been pending..."

              else
                attribute "title" ("Launch an indexation in mode " ++ masterModeStr)
            ]
            [ text ("Indexation " ++ masterModeStr) ]
        ]


displayCallGetWorkflows : Operation -> Bool -> Html Msg
displayCallGetWorkflows op display =
    let
        displayMode =
            if op.operationId == -1 then
                "none"

            else
                "block"
    in
    div
        [ style "display" displayMode
        ]
        [ button [ onClick CallGetWorkflows ] [ text ("Publication Workflows " ++ fromBoolToString display) ]
        ]


displayCallTasks : Operation -> Bool -> Html Msg
displayCallTasks op display =
    let
        displayMode =
            if op.operationId == -1 then
                "none"

            else
                "block"
    in
    div
        [ style "display" displayMode ]
        [ button [ onClick CallGetTasks ] [ text ("Tasks " ++ fromBoolToString display) ] ]


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


displayMenu : Operation -> Bool -> Bool -> Html Msg
displayMenu op isDispTask isDispWorkflow =
    let
        displayMode =
            if op.operationId == -1 then
                "none"

            else
                "block"
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , div [ style "display" "flex" ]
            [ div [ style "flex" "1" ] [ displayCallTasks op isDispTask ]
            , div [ style "flex" "1" ] [ displayCallGetWorkflows op isDispWorkflow ]
            ]
        ]


displayHeader : Html Msg
displayHeader =
    div [ style "display" "flex", style "flex-direction" "row" ]
        [ div [ style "flex" "1", style "white-space" "nowrap" ] [ displayEnv ]
        , div [ style "flex" "50" ] []
        , div [ style "flex" "1", style "white-space" "nowrap" ] [ displayStats ]
        ]


displayEnv : Html Msg
displayEnv =
    div []
        [ text ("Environment : " ++ fromEnvToString env) ]


displayStats : Html Msg
displayStats =
    div []
        [ a [ target "_blank", href kibanaUrl ]
            [ text "Statistics on Kibana" ]
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
        , button [ onClick CallGetOperation ]
            [ text "OK" ]
        ]



---------- TRACKING


track : String -> Attribute msg
track label =
    attribute "data-vpa-id" label



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
    in
    { title = "Pamela " ++ version
    , body =
        [ div [ style "margin" "20px" ]
            [ displayHeader
            , displayInputOperation model.operationInput
            , displayOperation modelOp
            , displayMenu modelOp model.displayTasks model.displayWorkflows
            , displayCallMasterMode modelOp (getLastTask modelTasks) model.displayTasks
            , displayMessageUser model.messageUser
            , displayWorkflows modelWorkflows model.displayWorkflows modelOp.operationCode
            , displayTasks modelTasks model.displayTasks modelOp.operationCode
            , displayFooter version
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
