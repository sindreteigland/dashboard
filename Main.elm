module Main exposing (..)

import Dom exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder, field, float, int, maybe, string, succeed)
import Keyboard exposing (..)
import Task
import Data
import WebSocket
import QRCode
import List.Extra exposing (..)
import Navigation exposing (..)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onClick)
-- APP
--main : Program Never Model Msg


main =
    Html.program
        { init = model
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias GridCell =
    { content : Html Msg, spanH : Int, spanV : Int }


type alias Model =
    { position : Int
    , activeNavBarItem : Int
    , currentPage : Page
    , navigator : NavigationMap
    , navigationPath : List ( String, Int )
    , pages : List Page
    }


type alias Page =
    { navBarIcon : String
    , gridSize : Int
    , pageName : String
    , moduleList : List GridCell
    }


model : ( Model, Cmd Msg )
model =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { position = 0
    , activeNavBarItem = 0
    , currentPage = Data.page1
    , navigator = defaultNavigator
    , navigationPath = [ ( "navBar", 0 ) ]
    , pages =
        [ Data.page1, Data.page2 ]
    }



-- getFocusElement navigationPath =
--     let
--         focusElement a b =
--             String.append a b
--     in
--         focusElement <| List.Extra.last navigationPath


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , WebSocket.listen "ws://localhost:3000" Receive
        ]


type Msg
    = SetFocus Int
    | FocusResult (Result Dom.Error ())
    | KeyMsg Keyboard.KeyCode
    | Receive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Debug log: " msg of
        KeyMsg code ->
            let
                newModel =
                    convertInputToCommand code model.navigator |> navigator model
            in
                ( { newModel | position = newModel.position }
                , toString newModel.position
                    |> Dom.focus
                    |> Task.attempt FocusResult
                )

        SetFocus value ->
            ( { model | position = value }, toString value |> Dom.focus |> Task.attempt FocusResult )

        FocusResult result ->
            -- handle success or failure here
            case result of
                Err (Dom.NotFound id) ->
                    -- unable to find dom 'id'
                    ( model, Cmd.none )

                Ok () ->
                    -- successfully focus the dom
                    ( model, Cmd.none )

        Receive message ->
            case JD.decodeString int message of
                Ok code ->
                    let
                        newModel =
                            convertInputToCommand code model.navigator |> navigator model
                    in
                        ( { newModel | position = newModel.position }, toString newModel.position |> Dom.focus |> Task.attempt FocusResult )

                Err error ->
                    let
                        err =
                            Debug.log "msg error" error
                    in
                        model ! []



-- VIEW


cell : Int -> GridCell -> Html Msg
cell index cellContent =
    div
        [ class "grid-cell"
        , tabindex index
        , id <| toString index
        , onClick <| SetFocus index
        , style [ ( "grid-row-end", "span " ++ toString cellContent.spanV ), ( "grid-column-end", "span " ++ toString cellContent.spanH ) ]
        ]
        [ cellContent.content
        ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ navBar model
        , mainGrid model
        , remote "http://192.168.0.100:8000"
        ]


qrCodeView : String -> Html msg
qrCodeView message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault
            (Html.text "Error while encoding to QRCode.")


remote url =
    div [ class "remote" ]
        [ qrCodeView url
        , img [ src "icons/icons8-remote-control-48.png" ] []
        ]


mainGrid model =
    div
        [ class "container"
        , style
            [ ( "grid-template-columns"
              , String.concat [ "repeat(", toString model.currentPage.gridSize, ",1fr)" ]
              )
            ]
        ]
        (List.indexedMap cell model.currentPage.moduleList)


activeNavBarItemStyle =
    [ ( "background-color", "#0092ee" )
    , ( "border-radius", "5px" )
    ]


navBar model =
    div [ class "navbar" ]
        (model.pages |> List.indexedMap (\i page -> navBarIcon i page model.activeNavBarItem))


navBarIcon index page activeNavBarItem =
    let
        iconStyle =
            if activeNavBarItem == index then
                activeNavBarItemStyle
            else
                []
    in
        div
            [ class "navbar-menu-icon"
            , id <| String.append "navbar" <| toString index
            , tabindex index
            , onClick <| SetFocus index
            ]
            [ div
                [ style
                    [ ( "height", "2em" )
                    , ( "width", "2px" )
                    , ( "align-self", "center" )
                    , ( "margin-right", "6px" )
                    ]
                ]
                []
            , img [ class "navbar-icon", src page.navBarIcon, style iconStyle ] []
            ]
