module Main exposing (main)

import Html
    exposing
        ( Html
        , text
        , div
        , nav
        , input
        , textarea
        , h1
        , a
        , ul
        , li
        , br
        , span
        , button
        )
import Html.Attributes exposing (class, value, href, placeholder)
import Html.Events exposing (onInput, onClick)
import Markdown
import Journal exposing (Journal, Entry, updateTitle, updateContent)
import Array exposing (Array)
import Ports
import Navigation

main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Flags =
    { date : String
    }

-- Model

type alias Model =
    { history : Navigation.Location
    , journal : Journal
    , viewState : ViewState
    , search : String
    , currentDate : String
    }

type ViewState
    = Listing
    | Viewing Int
    | Editing Int Entry
    | Creating Entry
    | NotFound


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( { journal = Journal.empty
      , viewState = Listing
      , search = ""
      , currentDate = flags.date
      , history = location
      }
    , Ports.loadJournal
    )


type Msg
    = ShowEntry Int
    | EditEntry Int
    | ListEntries
    | EditSearch String
    | NewEntry
    | UpdateEntryTitle String
    | UpdateEntryDate String
    | UpdateEntryContent String
    | SaveEntry
    | JournalUpdated Journal
    | UnknownData String
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange loc ->
            ( { model | viewState = Listing }, Cmd.none )
        ShowEntry pos ->
            ( { model | viewState = Viewing pos }, Cmd.none )

        ListEntries ->
            ( { model | viewState = Listing }, Cmd.none )
        EditSearch wanted ->
            ( { model | search = wanted }, Cmd.none )
        EditEntry pos ->
            case Journal.getEntry pos model.journal of
                Just entry ->
                    ( { model | viewState = Editing pos entry }, Cmd.none )

                Nothing ->
                    ( { model | viewState = NotFound }, Cmd.none )

        NewEntry ->
            ( { model | viewState = Creating (Entry "" model.currentDate "") }, Cmd.none )

        SaveEntry ->
            case model.viewState of
                Editing pos entry ->
                    saveJournal model (Journal.updateEntry pos entry model.journal)

                Creating entry ->
                    saveJournal model (Journal.addEntry entry model.journal)

                _ ->
                    ( model, Cmd.none )

        UpdateEntryTitle newTitle ->
            let
                newViewState =
                    updateEditingState model.viewState (Journal.updateTitle newTitle)
            in
                ( { model | viewState = newViewState }, Cmd.none )
        UpdateEntryDate newDate ->
                        let
                            newViewState =
                                updateEditingState model.viewState (Journal.updateDate newDate)
                        in
                            ( { model | viewState = newViewState }, Cmd.none )
        UpdateEntryContent newContent ->
            let
                newViewState =
                    updateEditingState model.viewState (Journal.updateContent newContent)
            in
                ( { model | viewState = newViewState }, Cmd.none )

        JournalUpdated journal ->
            let
                newView =
                    case model.viewState of
                        Editing pos _ ->
                            Viewing pos

                        Creating _ ->
                            Listing

                        _ ->
                            model.viewState
            in
                ( { model
                    | journal = journal
                    , viewState = newView
                  }
                , Cmd.none
                )

        -- unrecognized message from js
        UnknownData description ->
            ( model, Cmd.none )


saveJournal : Model -> Journal -> ( Model, Cmd Msg )
saveJournal model newJournal =
    ( model, Ports.saveJournal newJournal )


updateEditingState : ViewState -> (Entry -> Entry) -> ViewState
updateEditingState viewState updateFn =
    case viewState of
        Editing pos entry ->
            Editing pos (updateFn entry)

        Creating entry ->
            Creating (updateFn entry)

        _ ->
            viewState



-- View


view : Model -> Html Msg
view model =
    case model.viewState of
        Listing ->
            listView model model.journal

        Viewing pos ->
            entryViewer pos model.journal

        Creating entry ->
            entryEditor SaveEntry ListEntries entry

        Editing pos entry ->
            entryEditor SaveEntry (ShowEntry pos) entry

        NotFound ->
            notFoundView

entryFilter : Entry -> String -> Bool
entryFilter entry w  =
    String.contains w entry.title || String.contains w entry.date


listView : Model -> Journal -> Html Msg
listView model journal =
    let
        entrySummary idx entry =
            li [ class "entry-summary" ]
            [ a [ class "title", onClick (ShowEntry idx), href "#" ] [ text entry.title ]
            , span [] [text (" - " ++ entry.date)]
            ]
    in
        div []
            [ button [ class "button-primary", onClick NewEntry ] [ text "New Entry" ]
            , br [] []
            , input [ placeholder "Search", onInput EditSearch] [] -- found way to access both models
            , ul [ class "journal" ]
                (Array.filter (\a -> entryFilter a model.search) journal
                |> Array.indexedMap entrySummary
                |> Array.toList
                )
            ]


entryEditor : Msg -> Msg -> Entry -> Html Msg
entryEditor onSave onCancel entry =
    div []
        [ div [ class "editor" ]
            [ div [ class "inputs" ]
                [ div [ class "title" ]
                    [ input [ onInput (UpdateEntryTitle), value entry.title ] []
                    ]
                , div [ class "date" ]
                    [ input [ onInput (UpdateEntryDate), value entry.date ] []
                    ]
                , div [ class "content" ]
                    [ textarea [ onInput (UpdateEntryContent), value entry.content ] []
                    ]
                , nav []
                    [ navLink onCancel "Cancel"
                    , button [ class "button-primary", onClick onSave ] [ text "Save" ]
                    ]
                ]
            , div [ class "preview" ]
                [ entryDisplay entry
                ]
            ]
        ]


entryViewer : Int -> Journal -> Html Msg
entryViewer pos journal =
    case Journal.getEntry pos journal of
        Just entry ->
            div []
                [ nav []
                    [ navLink ListEntries "< Back"
                    , navLink (EditEntry pos) "Edit"
                    ]
                , entryDisplay entry
                ]

        Nothing ->
            notFoundView


entryDisplay : Entry -> Html Msg
entryDisplay entry =
    div [ class "entry" ]
        [ h1 [ class "title" ] [ text entry.title ]
        , div [ class "date" ]
            [ Markdown.toHtml [] entry.date ]
        , div [ class "content" ]
            [ Markdown.toHtml [] entry.content ]
        ]


notFoundView : Html Msg
notFoundView =
    div []
        [ h1 [] [ text "Not found" ]
        , navLink ListEntries "Back to journal"
        ]


navLink : Msg -> String -> Html Msg
navLink msg content =
    a [ onClick msg, href "#" ] [ text content ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.journalUpdates JournalUpdated UnknownData
