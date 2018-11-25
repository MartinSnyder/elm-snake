module Grid exposing (..)

type Direction = Up | Down | Left | Right

type alias Size =
  { width : Int
  , height : Int
  }

type alias Position =
  { x : Int
  , y : Int
  }

type alias Offset =
  { dx : Int
  , dy : Int
  }

directionToOffset : Direction -> Offset
directionToOffset dir =
  case dir of
    Up -> Offset 0 -1
    Down -> Offset 0 1
    Left -> Offset -1 0
    Right -> Offset 1 0

applyOffset : Position -> Offset -> Position
applyOffset pos offset = Position (pos.x + offset.dx) (pos.y + offset.dy)

adjustPosition: Position -> Direction -> Position
adjustPosition pos dir = applyOffset pos (directionToOffset dir)

computeGridCenter : Size -> Position
computeGridCenter size = Position (size.width // 2) (size.height // 2)

computePointsInGrid : Size -> List Position
computePointsInGrid size =
  let
    allX = List.range 0 (size.width - 1)
    allY = List.range 0 (size.height - 1)
  in
  List.concatMap (\x -> List.map (\y -> (Position x y)) allY) allX

isInGrid : Size -> Position -> Bool
isInGrid size pos =
  pos.x >= 0
  && pos.y >= 0
  && pos.x < size.width
  && pos.y < size.height