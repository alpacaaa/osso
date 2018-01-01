module Examples.Benchmark where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random as Random
import Control.Monad.Rec.Class as Rec
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Debug

import Html as Html
import Html (Html, div, a, h1, span, button, table, tbody, td, tr, text)
import Html.Attributes (id, classes, classList, attribute, type_, href)
import Html.Events (onClick)

-- import Debug.Trace as Debug


type Model =
    { rows :: Array Row
    , lastId :: Int
    }

data Msg
    = Create Int
    | Append Int
    | UpdateEvery Int
    | Clear
    | Swap
    | Remove Int
    | Select Int
    | RowsCreated (Array Row)
    | RowsAppended (Array Row)

data Effect next
    = RandomRows Int Int (Array Row -> next)

type Row =
    { id :: Int
    , label :: String
    , selected :: Boolean
    }

type RowStep =
    { id :: Int, amount :: Int, rows :: Array Row }

type Button =
    { buttonId :: String, labelText :: String, msg :: Msg }


init :: { state :: Model, effects :: Array (Effect Msg) }
init =
    Html.noFx { rows: [], lastId: 0 }



update :: Msg -> Model -> { state :: Model, effects :: Array (Effect Msg) }
update msg model =
    case msg of
        Create amount ->
            createRows model amount RowsCreated

        Append amount ->
            createRows model amount RowsAppended
        
        RowsCreated newRows ->
            Html.noFx $ model { rows = newRows }

        RowsAppended newRows ->
            Html.noFx $ model { rows = model.rows <> newRows }

        UpdateEvery amount ->
            Html.noFx $ model { rows = Array.mapWithIndex updateRow model.rows }

        Clear ->
            Html.noFx $ model { rows = [] }

        Swap ->
            if Array.length model.rows > 998 then
                let
                    swapped = do
                        a <- Array.index model.rows 1
                        b <- Array.index model.rows 998

                        Array.insertAt 1 b model.rows
                            >>= (Array.insertAt 998 a)

                in
                case swapped of
                    Just rows -> Html.noFx $ model { rows = rows }
                    Nothing   -> Debug.unsafeCrashWith "Something's wrong"
            else
                Html.noFx model

        Remove id ->
            Html.noFx $ model { rows = Array.filter (\r -> r.id /= id) model.rows }

        Select id ->
            Html.noFx $ model { rows = map (select id) model.rows }



effectManager :: Html.EffectManager Msg (Effect Msg) _
effectManager (RandomRows amount lastId next) = do
    rows <- Rec.tailRecM createRow { id: lastId, amount, rows: [] }
    pure [next rows]


program :: Eff _ (Html.Program Msg Model (Effect Msg) _)
program =
    Html.program
        { view
        , update
        , init
        , subscriptions: []
        , effectManager
        }


adjectives :: Array String
adjectives =
    [ "pretty"
    , "large"
    , "big"
    , "small"
    , "tall"
    , "short"
    , "long"
    , "handsome"
    , "plain"
    , "quaint"
    , "clean"
    , "elegant"
    , "easy"
    , "angry"
    , "crazy"
    , "helpful"
    , "mushy"
    , "odd"
    , "unsightly"
    , "adorable"
    , "important"
    , "inexpensive"
    , "cheap"
    , "expensive"
    , "fancy"
    ]


colours :: Array String
colours =
    [ "red"
    , "yellow"
    , "blue"
    , "green"
    , "pink"
    , "brown"
    , "purple"
    , "brown"
    , "white"
    , "black"
    , "orange"
    ]


nouns :: Array String
nouns =
    [ "table"
    , "chair"
    , "house"
    , "bbq"
    , "desk"
    , "car"
    , "pony"
    , "cookie"
    , "sandwich"
    , "burger"
    , "pizza"
    , "mouse"
    , "keyboard"
    ]



buttons :: Array Button
buttons =
    [ { buttonId: "run", labelText: "Create 1,000 rows", msg: (Create 1000) }
    , { buttonId: "runlots", labelText: "Create 10,000 rows", msg: (Create 10000) }
    , { buttonId: "add", labelText: "Append 1,000 rows", msg: (Append 1000) }
    , { buttonId: "update", labelText: "Update every 10th row", msg: (UpdateEvery 10) }
    , { buttonId: "clear", labelText: "Clear", msg: Clear }
    , { buttonId: "swaprows", labelText: "Swap Rows", msg: Swap }
    ]


btnPrimaryBlock :: Button -> Html Msg
btnPrimaryBlock { buttonId, labelText, msg } =
    div
        [ classes "col-sm-6 smallpad" ]
        [ button
            [ type_ "button"
            , classes "btn btn-primary btn-block"
            , id buttonId
            , onClick msg
            ]
            [ text labelText ]
        ]


viewRow :: Row -> Html Msg
viewRow { id, label, selected } =
    tr
        [ classList [ Tuple "danger" selected ] ]
        [ td [ classes "col-md-1" ] [ text (show id) ]
        , td
            [ classes "col-md-4" ]
            [ a
                [ href "#"
                , onClick (Select id)
                ]
                [ text label ]
            ]
        , td
            [ classes "col-md-1" ]
            [ a
                [ href "#"
                , onClick (Remove id)
                ]
                [ span
                    [ classes "glyphicon glyphicon-remove"
                    , attribute "aria-hidden" "true"
                    ]
                    []
                ]
            ]
        , td [ classes "col-md-6" ] []
        ]


view :: Model -> Html Msg
view model =
    div
        [ classes "container" ]
        [ div
            [ classes "jumbotron" ]
            [ div
                [ classes "row" ]
                [ div
                    [ classes "col-md-6" ]
                    [ h1
                        []
                        [ text "Elmish Purescript" ]
                    ]
                , div
                    [ classes "col-md-6" ]
                    (map btnPrimaryBlock buttons)
                ]
            ]
        , table
            [ classes "table table-hover table-striped test-data" ]
            [ tbody
                []
                (map viewRow model.rows)
            ]
        , span
            [ classes "preloadicon glyphicon glyphicon-remove"
            , attribute "aria-hidden" "true"
            ]
            []
        ]


createRows :: Model -> Int -> (Array Row -> Msg) -> { state :: Model, effects :: Array (Effect Msg) }
createRows model amount msg =
    let
        fx =
            RandomRows amount model.lastId msg
    in
    model { lastId = model.lastId + amount }
    # Html.withFx [fx]


generateLabel :: Eff _ String
generateLabel = do
    adj <- Array.index adjectives <$> (Random.randomInt 1 $ Array.length adjectives - 1)
    clr <- Array.index colours    <$> (Random.randomInt 1 $ Array.length colours - 1)
    non <- Array.index nouns      <$> (Random.randomInt 1 $ Array.length nouns - 1)

    let label = adj <> Just " " <> clr <> Just " " <> non

    pure $ case label of
        Just l  -> l
        Nothing -> Debug.unsafeCrashWith "Something's wrong"


updateRow :: Int -> Row -> Row
updateRow index row =
    if index `mod` 10 == 0 then
        row { label = row.label <> " !!!" }
    else
        row


select :: Int -> Row -> Row
select targetId row@{ id, label, selected } =
    if id == targetId then
        row { selected = true }
    else if selected == true then
        row { selected = false }
    else
        row


createRow :: RowStep -> Aff _ (Rec.Step RowStep (Array Row))
createRow { amount: 0, rows }  = pure (Rec.Done rows)
createRow { id, amount, rows } = do
    label <- liftEff generateLabel

    let newRow =
           { id: id + amount, label, selected: false }

    pure $ Rec.Loop
        { id
        , amount: amount - 1
        , rows: Array.cons newRow rows
        }
