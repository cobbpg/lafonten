{-# LANGUAGE DataKinds #-}

module LambdaCube.Font.Common where

import Data.Vect
import LambdaCube.GL

type OutlineCurves = [[Vec2]]

turn :: Vec2 -> Vec2
turn (Vec2 x y) = Vec2 y (-x)

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

v2v3 :: Exp s V2F -> Exp s V3F
v2v3 v = let V2 x y = unpack' v in pack' $ V3 x y (Const 1)

v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)
