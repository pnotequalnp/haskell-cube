{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Cube (tests) where

import Data.Foldable (fold)

import Test.Tasty
import Test.Tasty.HUnit

import Cube

tests :: forall c. (Cube c, Show c) => TestName -> TestTree
tests = flip testGroup $
  [ inverseTests @c
  , periodTests @c
  , tripleIsInverseTests @c
  , tPermTests @c
  ]

inverseTests :: forall c. (Cube c, Show c) => TestTree
inverseTests = testGroup "Inverse moves" $
  uncurry (inverseMatches @c) <$> inverses

periodTests :: forall c. (Cube c, Show c) => TestTree
periodTests = testGroup "Period" $
     (periodicN @c 4 <$> quarterTurns)
  <> (periodicN @c 2 <$> halfTurns)

tripleIsInverseTests :: forall c. (Cube c, Show c) => TestTree
tripleIsInverseTests = testGroup "Triple is inverse" $
  tripleIsInverse @c <$> faceTurns

tPermTests :: forall c. (Cube c, Show c) => TestTree
tPermTests = testGroup "T perms match" $
  zipWith test cs [1 :: Int ..]
  where
  ts = [ [R, U, R', U', R', F, R2, U', R', U', R, U, R', F']
       , [R, U, R', U', R', F, R2, U', R', U, F', L', U, L, U2]
       , [R2, U, R2, U', R2, U', D, R2, U', R2, U, R2, D']
       , [R', U', R, U, R, B', R2, U, R, U, R', U', R, B]
       ]
  c:cs = foldMap (fromMove @c) <$> ts 
  test t i = testCase ("T perms match (" <> show i <> ")") $ t @?= c

quarterTurns :: [Move]
quarterTurns = [U, U', F, F', R, R', B, B', L, L', D, D']

halfTurns :: [Move]
halfTurns = [U2, F2, R2, B2, L2, D2]

faceTurns :: [Move]
faceTurns = quarterTurns <> halfTurns

inverses :: [(Move, Move)]
inverses =
  [ (U, U'), (U2, U2), (U', U)
  , (F, F'), (F2, F2), (F', F)
  , (R, R'), (R2, R2), (R', R)
  , (B, B'), (B2, B2), (B', B)
  , (L, L'), (L2, L2), (L', L)
  , (D, D'), (D2, D2), (D', D)
  ]

inverseMatches :: forall c. (Cube c, Show c) => Move -> Move -> TestTree
inverseMatches m m' = testCase ("Inverse matches (" <> show m <> ")") $
  (invert . fromMove @c) m @?= fromMove @c m'

tripleIsInverse :: forall c. (Cube c, Show c) => Move -> TestTree
tripleIsInverse m = testCase ("Triple is inverse (" <> show m <> ")") $
  (fold . replicate 3) c @?= invert c
  where c = fromMove @c m

periodicN :: forall c. (Cube c, Show c) => Int -> Move -> TestTree
periodicN n m = testCase ("Periodic at " <> show n <> " (" <> show m <> ")") $
  (fold . replicate n) (fromMove @c m) @?= mempty