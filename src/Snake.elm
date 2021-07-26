import Browser
import Browser.Events
import Html exposing (Html, div, input)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Random
import Grid exposing (..)
import Util exposing (..)
import Json.Decode as Decode
import NonEmptyList as NEL exposing (NonEmptyList)
import Html.Events.Extra.Pointer as Pointer
import Html.Events exposing (on, keyCode)
import Browser.Navigation exposing (Key)

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- CONSTANTS
gridSize = Size 40 20
cellSize = Size 20 20
tickFrequency = 100
initialSnakeLength = 20

-- MODEL
type State = Active | Inactive

type alias Model =
  { state : State
  , gameTicks : Int
  , direction : Direction
  , snake : NonEmptyList Position
  , prize : (Maybe Position)
  , score : Int
  , highScore : Int
  }

initGame : State -> Int -> (Model, Cmd Msg)
initGame initialState highScore =
  let
    head = computeGridCenter gridSize
    initSnake = NonEmptyList head (List.repeat (initialSnakeLength - 1) head)
  in
  ( { state = initialState
    , gameTicks = 0
    , direction = Up
    , snake = initSnake
    , prize = Nothing
    , score = 0
    , highScore = highScore
    }
  , if (initialState == Active) then placePrize initSnake else Cmd.none
  )

init : () -> (Model, Cmd Msg)
init _ = initGame Inactive 0

-- UPDATE
type Msg = Tick Time.Posix | PlacePrize (Maybe Position) | PointerDownAt ( Float, Float ) | KeyDown Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if (model.state == Active) then
    case msg of
      PointerDownAt offsetPos ->
        ( { model | direction = pointerOffsetToDirection offsetPos model.direction model.snake.head }
        , Cmd.none
        )

      KeyDown key ->
        case key of
          37 -> 
            if model.direction /= Right then 
              ( { model | direction = Left }
              , Cmd.none
              )
            else (model, Cmd.none)
          38 ->
            if model.direction /= Down then 
              ( { model | direction = Up }
              , Cmd.none
              )
            else (model, Cmd.none)
          39 ->
            if model.direction /= Left then 
              ( { model | direction = Right }
              , Cmd.none
              )
            else (model, Cmd.none)
          40 ->
            if model.direction /= Up then 
              ( { model | direction = Down }
              , Cmd.none
              )
            else (model, Cmd.none)
          _ -> (model, Cmd.none)

      Tick time ->
        let
          nextHead = adjustPosition model.snake.head model.direction
          atePrize = (Just nextHead) == model.prize
          nextScore = if atePrize then model.score + 1 else model.score
          nextTail = model.snake.head :: if atePrize then model.snake.tail else stripLast model.snake.tail
          nextSnake = NonEmptyList nextHead nextTail
          nextState = if (isLegalState nextSnake) then Active else Inactive
          nextModel =
            { model
              | state = nextState
              , snake = nextSnake
              , score = nextScore
              , highScore = Basics.max nextScore model.highScore
              , gameTicks = if (nextState == Active) then model.gameTicks + 1 else (-1000 // tickFrequency) // 2
            }
        in
          ( nextModel , if atePrize then placePrize nextSnake else Cmd.none )

      PlacePrize  pos ->
        ( { model | prize = pos }, Cmd.none )

    else
      case msg of 
        PointerDownAt _ -> if (model.gameTicks >= 0) then initGame Active model.highScore else ( model, Cmd.none )
        Tick time ->
          ({ model | gameTicks = model.gameTicks + 1}, Cmd.none )

        KeyDown key ->
          if key == 13 then
            if (model.gameTicks >= 0) then initGame Active model.highScore else ( model, Cmd.none )
          else
            (model, Cmd.none)

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

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Decode.map tagger keyCode)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Time.every tickFrequency Tick

-- VIEW
view : Model -> Html Msg
view model =
  div []
  [
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
      ++ [ text_ [ x "5", y "20", Svg.Attributes.style "fill: white"] [ text ("Score: " ++ (String.fromInt model.score))]
         , text_ [ x (String.fromInt ((gridSize.width * cellSize.width) - 5)), y "20", Svg.Attributes.style "fill: white; text-anchor: end"] [ text ("High Score: " ++ (String.fromInt model.highScore))]
         ]
      ++ if (model.state == Inactive && model.gameTicks >= 0) then [ text_ [ x "50%", y "50%", Svg.Attributes.style "dominant-baseline:middle; text-anchor:middle; fill: white; font-size: large"] [ text "Click or touch to begin..." ] ] else []
      )
    , div [] [text ("Enter to start. Arrow keys to control"), input [onKeyDown KeyDown] []]
  ]

renderCircle : String -> Position -> Html Msg
renderCircle color pos =
  circle [ cx (String.fromInt ((pos.x * cellSize.width) + (cellSize.width // 2)))
         , cy (String.fromInt ((pos.y * cellSize.height) + (cellSize.height // 2)))
         , r (String.fromInt (cellSize.height // 2))
         , fill color
         ] []