module Tictactoe where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal as S
import List as L
import List.Extra as LE
import Maybe as M
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

-- convertTo2D : List Square -> List (List Square)
convertTo2D board = 
  let 
    splitEvery n list = 
      case list of
        [] -> []
        list -> let (first, rest) = LE.splitAt n list
                in first :: (splitEvery n rest)
  in
    splitEvery 3 board

type SquareStatus = Empty | X | O

type alias Square = {status : SquareStatus, id : Int}

newSquare : Square
newSquare = { status = Empty,
              id = 0 }

type alias Model = { board : Board,
                     turn : Player,
                     winner : Maybe Player,
                     gameOver : Bool }

initialModel : Model
initialModel = {
    board = L.map2 (\sq nId -> {sq | id <- nId} ) (L.repeat 9 newSquare) [1..9],
    turn = Red,
    winner = Nothing,
    gameOver = False
  }

--UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    None -> model
    MakeMove id ->
      let
        updateStatus square = 
          if square.id==id then 
            {square | status <- newStatus} 
          else 
            square
        newStatus = case model.turn of 
          Red -> X
          Blue -> O
        updatedBoard = L.map updateStatus model.board
        updated2DBoard = convertTo2D updatedBoard
        checkForWinner = M.oneOf [ updated2DBoard 
                                     |> LE.transpose 
                                     |> checkVerticals, 
                                   updated2DBoard 
                                     |> checkHorizontals,
                                   updated2DBoard
                                     |> checkDiagonals ]
        checkVerticals board = Nothing
        checkHorizontals board = Nothing
        checkDiagonals board = Nothing
      in {model | board <- updatedBoard,
                  turn <- (toggleTurn model.turn),
                  winner <- checkForWinner,
                  gameOver <- ((L.map .status updatedBoard)
                              |> L.member Empty |> not )
                              || checkForWinner /= Nothing}

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
    winnerToString : Maybe Player -> String
    winnerToString winner =
      case winner of
        Nothing -> "Nobody won.. -.-"
        Just Red -> "Red won! XD"
        Just Blue -> "Blue won! XD"

  in
    case model.gameOver of
      False ->
        div [] [
          div [id "board"] (L.map makeSquare model.board)
        ]
      True ->
        div [] [
          ul [class "notice"]
            [ li [] [text "game over"],
              li [] [text <| winnerToString model.winner],
              li [] [text "click refresh to try again"] ]
        ]

main : Signal Html
main = StartApp.start { model = initialModel,
                        update = update,
                        view = view }