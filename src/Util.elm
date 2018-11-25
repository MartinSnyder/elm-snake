module Util exposing (..)

maybeToList : Maybe a -> List a
maybeToList maybe =
  case maybe of
    Just a -> [ a ]
    Nothing -> []

stripLast : List a -> List a
stripLast list = List.take ((List.length list) - 1) list