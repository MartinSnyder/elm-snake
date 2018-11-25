import Browser
import Browser.Events
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Random
import Grid exposing (..)
import Util exposing (..)
import NonEmptyList as NEL exposing (NonEmptyList)
import Html.Events.Extra.Pointer as Pointer

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- CONSTANTS
gridSize = Size 40 20
cellSize = Size 20 20
tickFrequency = 100
initialSnakeLength = 20

-- MODEL
type State = Active | Inactive

type alias Model =
  { state: State
  , gameTicks : Int
  , direction : Direction
  , snake : NonEmptyList Position
  , prize : (Maybe Position)
  }

initGame : State -> (Model, Cmd Msg)
initGame initialState =
  let
    head = computeGridCenter gridSize
    initSnake = NonEmptyList head (List.repeat (initialSnakeLength - 1) head)
  in
  ( { state = initialState
    , gameTicks = 0
    , direction = Up
    , snake = initSnake
    , prize = Nothing
    }
  , if (initialState == Active) then placePrize initSnake else Cmd.none
  )

init : () -> (Model, Cmd Msg)
init _ = initGame Inactive

-- UPDATE
type Msg = Tick Time.Posix | PlacePrize (Maybe Position) | PointerDownAt ( Float, Float )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if (model.state == Active) then
    case msg of
      PointerDownAt offsetPos ->
        ( { model | direction = pointerOffsetToDirection offsetPos model.direction model.snake.head }
        , Cmd.none
        )

      Tick time ->
        let
          nextHead = adjustPosition model.snake.head model.direction
          atePrize = (Just nextHead) == model.prize
          nextTail = model.snake.head :: if atePrize then model.snake.tail else stripLast model.snake.tail
          nextSnake = NonEmptyList nextHead nextTail
          nextState = if (isLegalState nextSnake) then Active else Inactive
          nextModel =
            { model
              | state = nextState
              , snake = nextSnake
              , gameTicks = model.gameTicks + 1
            }
        in
          ( nextModel , if atePrize then placePrize nextSnake else Cmd.none )

      PlacePrize  pos ->
        ( { model | prize = pos }, Cmd.none )

    else
      case msg of 
        PointerDownAt _ -> initGame Active
        _ -> ( model, Cmd.none )

isLegalState : NonEmptyList Position -> Bool
isLegalState snake = (isInGrid gridSize snake.head) && not (List.member snake.head snake.tail)

placePrize : NonEmptyList Position -> Cmd Msg
placePrize snake =
  let
    allPoints = computePointsInGrid gridSize
    snakePoints = NEL.toList snake
    validPoints = List.filter (\p -> not (List.member p snakePoints)) allPoints
  in
  Random.generate PlacePrize (Random.map (\i -> List.head (List.drop i validPoints)) (Random.int 0 (List.length validPoints - 1)))

pointerOffsetToDirection : ( Float, Float ) -> Direction -> Position -> Direction
pointerOffsetToDirection eventOffset currentDirection snakeHead =
  let
    (eventX, eventY) = eventOffset
    dx = eventX - ((toFloat snakeHead.x + 0.5) * toFloat cellSize.width)
    dy = eventY - ((toFloat snakeHead.y + 0.5) * toFloat cellSize.height)
  in
  if (currentDirection == Up || currentDirection == Down) then
    if (dx < 0) then Left else Right
  else
    if (dy < 0) then Up else Down

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Time.every tickFrequency Tick

-- VIEW
view : Model -> Html Msg
view model =
  svg [ width "100%"
      , height "auto"
      , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
      , Pointer.onDown (\event -> PointerDownAt event.pointer.offsetPos)
      , Svg.Attributes.style "touch-action: none"
      ]
      (  rect [ width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height))] []
      :: (maybeToList model.prize |> List.map (\pos -> renderCircle "green" pos))
      ++ List.map (renderCircle "red") model.snake.tail
      ++ [ renderCircle "purple" model.snake.head ]
      ++ if (model.state == Inactive) then [ text_ [ x "50%", y "50%", Svg.Attributes.style "dominant-baseline:middle; text-anchor:middle; fill: white; font-size: large"] [ text "Click or touch to begin..." ] ] else []
      )

renderCircle : String -> Position -> Html Msg
renderCircle color pos =
  circle [ cx (String.fromInt ((pos.x * cellSize.width) + (cellSize.width // 2)))
         , cy (String.fromInt ((pos.y * cellSize.height) + (cellSize.height // 2)))
         , r (String.fromInt (cellSize.height // 2))
         , fill color
         ] []