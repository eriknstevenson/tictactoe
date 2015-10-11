module Tictactoe where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array as A
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

convertTo2D : List a -> Int -> List (List a)
convertTo2D board size = 
  let 
    splitEvery n list = 
      case list of
        [] -> []
        list -> let (first, rest) = LE.splitAt n list
                in first :: (splitEvery n rest)
  in
    splitEvery size board

(!!) : List a -> Int -> M.Maybe a
xs !! n = 
  if | n < 0     -> Nothing
     | otherwise -> case (xs,n) of
         ([],_)    -> Nothing
         (x::xs,0) -> Just x
         (_::xs,n) -> xs !! (n-1)

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

        moveX = (id-1) % 3
        moveY = ((toFloat id)-1) / 3 |> floor

        updatedBoard = L.map updateStatus model.board

        updatedBoardStatus = L.map .status updatedBoard

        convertedBoard = convertTo2D updatedBoardStatus 3

        isThereAWinner = checks
                         |> L.filter (\n->n>=2)
                         |> L.isEmpty
                         |> not

        checkForWinner = case isThereAWinner of
          True -> Just model.turn
          False -> Nothing
       
        checks = [ checkVerticals, 
                   checkHorizontals, 
                   checkDiagonalsA, 
                   checkDiagonalsB ]
        
        checkVerticals = L.sum [ checkDir (moveX, moveY) (0, -1) 0,
                                 checkDir (moveX, moveY) (0, 1) 0 ]

        checkHorizontals = L.sum [ checkDir (moveX, moveY) (-1, 0) 0,
                                   checkDir (moveX, moveY) (1, 0) 0 ]

        checkDiagonalsA = L.sum [ checkDir (moveX, moveY) (-1, -1) 0,
                                  checkDir (moveX, moveY) (1, 1) 0 ]
        
        checkDiagonalsB = L.sum [ checkDir (moveX, moveY) (1, -1) 0,
                                  checkDir (moveX, moveY) (-1, 1) 0 ]
        
        checkDir : (Int, Int) -> (Int, Int) -> Int -> Int
        checkDir (x, y) (dx, dy) total = 
          case (M.withDefault [] (convertedBoard !! (y+dy)) !! (x+dx)) of
            Nothing -> total
            Just sqr -> 
              if sqr==newStatus then
                checkDir (x+dx, y+dy) (dx,dy) (total+1)
              else
                total
        
        updatedGameOver = (updatedBoardStatus |> LE.notMember Empty ) 
                          || checkForWinner /= Nothing

      in case model.gameOver of
        False -> {model | board <- updatedBoard,
                          turn <- (toggleTurn model.turn),
                          winner <- checkForWinner,
                          gameOver <- updatedGameOver}
        True -> model

-- VIEW

view : S.Address Action -> Model -> Html
view address model = 
  let

    makeSquare : Square -> Html
    makeSquare square =
      case square.status of
        Empty -> div [ class "empty",
                 onClick address (MakeMove square.id) ] []
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
    
        div [] [
          div [id "board"] (L.map makeSquare model.board),
          div [class "notice"] (case model.gameOver of
            False -> [text "Tictactoe"]
            True ->
              [ ul []
                [ li [] [text "game over"],
                  li [] [text <| winnerToString model.winner],
                  li [] [text "click refresh to try again"] ]
              ]
            )
        ]
      

main : Signal Html
main = StartApp.start { model = initialModel,
                        update = update,
                        view = view }
