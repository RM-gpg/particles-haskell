module Main (main) where

import Types
import Visualiser
import Graphics.Gloss.Data.Color

main :: IO ()
main = visualise ps 1

ps :: [Particle]
ps = [Particle 1 5 (0,0) (0,0) blue, Particle 1 5 (-10,0) (5,0) blue, Particle 1 5 (0,-10) (0,2) blue, Particle 1 5 (0,10) (0,-10) blue]
