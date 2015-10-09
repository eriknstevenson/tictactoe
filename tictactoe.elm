module Tictactoe where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal as S
import List as L
import StartApp.Simple as StartApp

-- MODEL

type Action = None | MakeMove Int

type Player = Red | Blue

toggleTurn : Player -> Player
toggleTurn player = 
  case player of
    Red -> Blue
    Blue -> Red

type alias Board = List Square

type SquareStatus = Empty | X | O
type alias Square = {status : SquareStatus, id : Int}

newSquare : Square
newSquare = 
          {
            status = Empty,
            id = 0
          }


type alias Model = 
  {
    board : Board,
    turn : Player
  }

initialModel : Model
initialModel = {
    board = L.map2 (\sqr id -> {sqr | id <- id} ) (L.repeat 9 newSquare) [1..9],
    turn = Red
  }

--UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    None -> model
    MakeMove id ->
      let
        newStatus = case model.turn of 
          Red -> X
          Blue -> O
        updateStatus square = if square.id==id then {square | status <- newStatus} else square
      in {model | board <- L.map updateStatus model.board,
                  turn <- (toggleTurn model.turn)}
-- VIEW

view : S.Address Action -> Model -> Html
view address model = 
  let
    makeSquare : Square -> Html
    makeSquare square =
      case square.status of
        Empty -> div [onClick address (MakeMove square.id), class "empty"] 
          []
        X -> div [class "x"]
          [text "X"]
        O -> div [class "o"]
          [text "O"]
      

  in
    div [] [
      div [id "board"] (L.map makeSquare model.board)
    ]

main : Signal Html
main = 
  StartApp.start {
    model = initialModel,
    update = update,
    view = view
  }