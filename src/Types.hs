module Types where

import Graphics.Gloss.Data.Color

data WorldState = WS { particles :: [Particle], mousePos :: (Float, Float), clickedParticle :: Bool, slider1Pos :: (Float, Float), slider1Clicked :: Bool, slider2Pos :: (Float, Float), slider2Clicked :: Bool }

data Particle = Particle { mass :: Float, radius :: Float, position :: (Float, Float), velocity :: (Float, Float), col :: Color }
	deriving (Show,Eq)