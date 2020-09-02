{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Test.Tasty

import qualified Test.Cube (tests)

import Cube.Facelet (FaceletCube)

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ Test.Cube.tests @FaceletCube "FaceletCube" ]
