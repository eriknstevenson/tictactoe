import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal as S
import List as L
import StartApp.Simple as StartApp

-- MODEL

type Action = None

type alias Model = 
  {
    name : String,
    board : Board
  }

type alias Board = List Square

type Square = Empty | X | O

initialModel : Model
initialModel = 
  {
    name = "tictactoe",
    board = [ Empty, Empty, X,
              Empty, O,     Empty,
              Empty, Empty, Empty ]
  }

--UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    None -> model

-- VIEW

view : S.Address Action -> Model -> Html
view address model = 
  let   
    drawSquare : Square -> Html
    drawSquare square = 
      case square of
        Empty -> text "-"
        X -> text "x"
        O -> text "o"

  in
    div [] [
      h1 [] [text model.name],
      div [] (L.map drawSquare model.board)
    ]

main : Signal Html
main = 
  StartApp.start {
    model = initialModel,
    update = update,
    view = view
  }