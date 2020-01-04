module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, hr, img, input, table, td, text, tr)
import Html.Attributes exposing (attribute, placeholder, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Header, emptyBody, expectJson, expectWhatever, get, header, jsonBody, request)
import Json.Decode exposing (Decoder, at, bool, field, int, list, map, map3, map5, map8, maybe, nullable, string)
import Json.Encode as Encode
import List exposing (head, reverse, sortBy)
import String exposing (fromInt)



---------- CONFIG


type Env
    = CI
    | PROD


env : Env
env =
    CI


type alias Config =
    { server : String
    , defaultOperation : String
    }


config : Config
config =
    if env == PROD then
        { server = "http://mediaapi.vpback.vpgrp.io/api/v1"
        , defaultOperation = "AMARTINI2" -- GNORWAY38
        }

    else
        { server = "http://mediaapi-ci.vpback.vpgrp.io/api/v1"
        , defaultOperation = "LADC5"
        }



---------- TYPES


type alias Model =
    { op : Operation
    , operationInput : String
    , tasks : Tasks
    , responseApi : ResponseApi
    , workflows : Workflows
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
    | DoAction (Result Http.Error ())



---------- INITIALIZE


init : ( Model, Cmd Msg )
init =
    ( { op = emptyOperation
      , operationInput = config.defaultOperation
      , tasks = [ emptyTask ]
      , responseApi = emptyResponseApi
      , workflows = [ emptyWorkflow ]
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
    , -- aborted="",
      -- ended="",
      created = ""
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
        , expect = expectWhatever DoAction
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
        , expect = expectWhatever DoAction
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
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DoAction r ->
            ( model, requestGetTasks opInput )

        SetOperationInput op ->
            ( { model | operationInput = op }, Cmd.none )

        CallGetTasks ->
            ( model, requestGetTasks opInput )

        CallGetOperation ->
            ( { model | tasks = [ emptyTask ], workflows = [ emptyWorkflow ] }, requestGetOperation opInput )

        CallSwitchDAMtoNAS ->
            ( model, requestPostDAMtoNAS modelOp )

        CallSwitchNAStoDAM ->
            ( model, requestPatchNAStoDAM modelOp )

        CallDeleteTask taskId ->
            ( model, requestDeleteTask taskId )

        CallGetWorkflows ->
            ( model, requestGetWorkflows modelOp )

        CallAbortWorkflow workflowId ->
            ( model, requestAbortWorkflow workflowId )

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

                        Err err ->
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

                        Err err ->
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

                        Err err ->
                            [ { id = "-1"
                              , user = "ERRORRRRR"
                              , status = "ERRORRRRR"
                              , created = "ERRORRRRR"
                              , started = "ERRRORRRRRRRRRRRRRR"
                              }
                            ]
            in
            ( { model | workflows = gotWorkflows }, Cmd.none )

        GotAbortWorkflow r ->
            ( model, requestGetWorkflows modelOp )



---------- DISPLAYS


displayWorkflows : Workflows -> Html Msg
displayWorkflows workflows =
    let
        workflowsSortedByStarted =
            reverse (sortBy .started workflows)

        lastWorkflow =
            getLastWorkflow workflowsSortedByStarted

        displayMode =
            if lastWorkflow.id == "-1" then
                "none"

            else
                "block"
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , div []
            (List.map
                (\w ->
                    table [ style "border" "solid", style "width" "100%" ]
                        [ tr []
                            [ td [ style "width" "30%" ] [ text "PUBLICATION WORKFLOWS" ]
                            , td [ style "width" "70%" ]
                                [ if w.id == lastWorkflow.id && w.status /= "ABORTED" && w.status /= "ENDED" then
                                    button [ onClick (CallAbortWorkflow w.id) ] [ text "Abort this Publication" ]

                                  else
                                    text ""
                                ]
                            ]
                        , tr [] [ td [] [ text "Id" ], td [] [ text w.id ] ]
                        , tr [] [ td [] [ text "User" ], td [] [ text w.user ] ]
                        , tr [] [ td [] [ text "Status" ], td [] [ text (w.status ++ " ") ] ]
                        , tr [] [ td [] [ text "Created" ], td [] [ text w.created ] ]
                        , tr [] [ td [] [ text "Started" ], td [] [ text w.started ] ]
                        ]
                )
                workflowsSortedByStarted
            )
        ]


displayTasks : Tasks -> Html Msg
displayTasks tasks =
    let
        lastTask =
            getLastTask tasks

        displayMode =
            if lastTask.id == -1 then
                "none"

            else
                "block"
    in
    div [ style "display" displayMode ]
        [ hr [] []
        , div []
            (List.map
                (\t ->
                    table [ style "border" "solid", style "width" "100%" ]
                        [ tr []
                            [ td [ style "width" "30%" ] [ text "TASKS" ]
                            , td [ style "width" "70%" ]
                                [ if t.status == "Pending" then
                                    button [ onClick (CallDeleteTask t.id) ] [ text "Delete this task : PENDING" ]

                                  else
                                    text ""
                                ]
                            ]
                        , tr [] [ td [] [ text "Id" ], td [] [ text (fromInt t.id) ] ]
                        , tr [] [ td [] [ text "master_mode" ], td [] [ text t.master_mode ] ]
                        , tr [] [ td [] [ text "status" ], td [] [ text t.status ] ]
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
        [ table [ style "border" "solid", style "width" "500px" ]
            [ tr [] [ td [] [ text "OperationId" ], td [ style "width" "250px" ] [ text (fromInt o.operationId) ] ]
            , tr [] [ td [] [ text "OperationCode" ], td [] [ text o.operationCode ] ]
            , tr [] [ td [] [ text "Master Mode" ], td [] [ text o.masterMode ] ]
            ]
        ]


displayCallMasterMode : Operation -> Task -> Html Msg
displayCallMasterMode op lastTask =
    let
        displayMode =
            if op.masterMode == "NONE" then
                "none"

            else
                "block"

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
    in
    div
        [ style "display" displayMode
        ]
        [ hr [] []
        , button
            [ attribute enableMasterModeSwitch ""
            , if enableMasterModeSwitch == "disabled" then
                attribute "title" "The Master Mode can not be changed because the STATUS of the last task is Pending..."

              else
                attribute "title" ("Switch the current Master Mode " ++ masterModeStr ++ " to " ++ antiMasterModeStr)
            , if antiMasterModeStr == "DAM" then
                onClick CallSwitchNAStoDAM

              else
                onClick CallSwitchDAMtoNAS
            ]
            [ text ("Switch " ++ masterModeStr ++ " to " ++ antiMasterModeStr) ]
        ]


displayCallGetWorkflows : Operation -> Html Msg
displayCallGetWorkflows op =
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
        [ button [ onClick CallGetWorkflows ] [ text "Display Publication Workflows" ]
        ]


displayCallTasks : Operation -> Html Msg
displayCallTasks op =
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
        [ text "TASKS : "
        , button [ onClick CallGetTasks ] [ text ("Voir les Tasks de " ++ op.operationCode) ]
        ]



---------- VIEW


view : Model -> Html Msg
view model =
    let
        modelOp =
            model.op

        modelTasks =
            model.tasks

        modelWorkflows =
            model.workflows
    in
    div [ style "margin" "20px" ]
        [ div []
            [ input [ onInput SetOperationInput, value model.operationInput, placeholder "Opération" ] []
            , button [ onClick CallGetOperation ] [ text "OK" ]
            ]
        , displayOperation model.op
        , displayCallMasterMode model.op (getLastTask modelTasks)
        , displayCallGetWorkflows model.op

        -- , displayCallTasks model.op
        , displayWorkflows modelWorkflows
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
