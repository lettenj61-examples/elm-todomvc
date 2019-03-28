port module Main exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>

-}

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm :: TodoMVC", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


port setStorage : Model -> Cmd msg


port download : () -> Cmd msg


port requestImport : String -> Cmd msg


port importJson : (Model -> msg) -> Sub msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage { newModel | uploadContent = "" }, cmds ]
    )



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : String
    , uploadContent : String
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , uid = 0
    , uploadContent = ""
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | Download
    | RequestImport
    | UpdateImport String
    | ImportComplete Model


type Sorter
    = TaskName



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            ( { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries ++ [ newEntry model.field model.uid ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }

                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (\t -> t.id /= id) model.entries }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (not << .completed) model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        Download ->
            ( model, download () )

        RequestImport ->
            ( model, requestImport model.uploadContent )

        UpdateImport content ->
            ( { model | uploadContent = content }
            , Cmd.none
            )

        ImportComplete loadedModel ->
            ( loadedModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    importJson ImportComplete



-- VIEW


view : Model -> Html Msg
view model =
    section
        [ class "section"

        --, style "visibility" "hidden"
        ]
        [ div
            [ class "container" ]
            [ lazy viewInput ( model.field, model.uploadContent )
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]


viewInput : ( String, String ) -> Html Msg
viewInput ( task, content ) =
    header
        []
        [ h1 [ class "title" ] [ text "todos" ]
        , wrapControl <|
            input
                [ class "input"
                , placeholder "What needs to be done?"
                , autofocus True
                , value task
                , name "newTodo"
                , onInput UpdateField
                , onEnter Add
                ]
                []
        , wrapControl <|
            input
                [ class "input"
                , value content
                , onInput UpdateImport
                ]
                []
        , div
            [ class "field is-grouped" ]
            [ wrapIn "p" "control" <|
                button
                    [ class "button"
                    , onClick RequestImport
                    ]
                    [ text "Upload" ]
            , wrapIn "p" "control" <|
                button
                    [ class "button is-link"
                    , onClick Download
                    ]
                    [ text "Download" ]
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


wrapIn : String -> String -> Html msg -> Html msg
wrapIn nodeType className node_ =
    node nodeType [ class className ] [ node_ ]


wrapControl : Html msg -> Html msg
wrapControl node_ =
    wrapIn "div" "field" <| wrapIn "p" "control" <| node_



-- VIEW ALL ENTRIES


viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"

        allToggleBox =
            input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
    in
    div
        [ class "field"
        , style "visibility" cssVisibility
        ]
        [ wrapControl <|
            label
                [ class "checkbox"
                , for "toggle-all"
                ]
                [ allToggleBox, text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list", id "todo-list" ] <|
            List.map viewKeyedEntry (List.filter isVisible entries)
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt todo.id, lazy viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    let
        editingStyle =
            if todo.editing then
                ""

            else
                "not-editing"
    in
    li
        []
        [ div
            [ class "field is-grouped" ]
            [ wrapIn "p" "control" <|
                input
                    [ type_ "checkbox"
                    , checked todo.completed
                    , onClick (Check todo.id (not todo.completed))
                    ]
                    []
            , wrapIn "p" "control is-expanded" <|
                input
                    [ class "input"
                    , class editingStyle
                    , disabled todo.completed
                    , value todo.description
                    , name "title"
                    , id ("todo-" ++ String.fromInt todo.id)
                    , onInput (UpdateEntry todo.id)

                    --, onBlur (EditingEntry todo.id False)
                    --, onEnter (EditingEntry todo.id False)
                    --, onFocus (EditingEntry todo.id True)
                    ]
                    []
            , button
                [ class "delete"
                , onClick (Delete todo.id)
                ]
                []
            ]
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    div
        [ class "field has-addons" ]
        [ visibilitySwap "#/" "All" visibility
        , visibilitySwap "#/active" "Active" visibility
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    wrapIn "p" "control" <|
        a
            [ onClick (ChangeVisibility visibility)
            , class "button"
            , href uri
            ]
            [ span [] [ text visibility ] ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "button is-warning"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
