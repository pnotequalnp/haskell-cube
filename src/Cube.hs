module Cube where

data Move =
    U | U2 | U'
  | F | F2 | F'
  | R | R2 | R'
  | B | B2 | B'
  | L | L2 | L'
  | D | D2 | D'
  deriving (Eq, Show)

class Monoid g => Group g where
  invert :: g -> g

class (Group c, Eq c) => Cube c where
  fromMove :: Move -> c

  solved :: c -> Bool
  solved = (==) mempty
