module NonEmptyList exposing (..)

type alias NonEmptyList a =
  { head : a
  , tail: List a
  }

cons : a -> NonEmptyList a -> NonEmptyList a
cons el nel = NonEmptyList el (nel.head :: nel.tail)

toList : NonEmptyList a -> List a
toList nel = nel.head :: nel.tail