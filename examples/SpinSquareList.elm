module SpinSquareList where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SpinSquare
import StartApp exposing (HandledTask)
import Dict exposing (Dict)
import Task


-- MODEL

type alias Model =
    { squares : Dict ID SpinSquare.Model
    , nextID : ID
    }

type alias ID = Int


init : (Model, List Action)
init =
    ( { squares = Dict.empty
      , nextID = 0
      }
    , [ InsertMany 100 ]
    )


-- UPDATE

type Action
    = Insert
    | InsertMany Int
    | Remove ID
    | Modify ID SpinSquare.Action
    | SpinThemAll


update : Signal.Address Action -> Action -> Model -> (Model, Maybe HandledTask)
update address action model =
    case action of
        Insert ->
            let
                ( newSquare, newActions ) = SpinSquare.init

            in
                ( { model
                      | squares <- Dict.insert model.nextID newSquare model.squares 
                      , nextID <- model.nextID + 1
                  }
                  -- Should really do something with newActions, but I happen to
                  -- know that it's Nothing
                , Nothing 
                )

        InsertMany howMany ->
            if howMany > 0
                -- Again, ignoring actions, which I happen to know will be Nothing
                then update address (InsertMany (howMany - 1)) (fst <| update address Insert model)
                else (model, Nothing)

        Remove id ->
            ( { model | squares <- Dict.remove id model.squares } 
            , Nothing
            )

        SpinThemAll ->
            let
                spin id =
                    Signal.send address (Modify id SpinSquare.Spin)
            
            in
                ( model
                , Just <|
                    Task.map (always ()) <|
                        Task.sequence <|
                            List.map spin <|
                                Dict.keys model.squares
                )
           
        Modify id subaction ->
            let
                maybeTask =
                    maybeNewSquareAndTask `Maybe.andThen` snd

                maybeNewSquareAndTask =
                    Maybe.map updateSquare (Dict.get id model.squares)
                
                newModel =
                    Maybe.withDefault model <|
                        Maybe.map updateModel maybeNewSquareAndTask
                
                updateSquare square =
                    SpinSquare.update (Signal.forwardTo address (Modify id)) subaction square
                
                updateModel squareAndTask =
                    { model | squares <- Dict.insert id (fst squareAndTask) model.squares }

            in
                ( newModel , maybeTask )


-- VIEW

(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address model =
    let
        viewSquare id square =
            SpinSquare.view (Signal.forwardTo address (Modify id)) square

        squares =
            List.map snd <|
                Dict.toList <|
                    Dict.map viewSquare model.squares
        
        heavy =
            [ "font-size" => "14pt"
            , "font-weight" => "bold"
            ]

    in
        div 
            [ style heavy ]
            [ text <| toString (List.length squares) ++ " squares "
            , button
                [ onClick address (InsertMany 10)
                , style heavy ]
                [ text " Add 10 " ]
            , button
                [ onClick address SpinThemAll 
                , style heavy
                ]
                [ text " Spin them all! " ] 
            , div
                [ style [ "display" => "flex" ] ]
                squares
            ]
