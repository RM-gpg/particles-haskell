module Visualiser where

import Types
import Simulation ( advanceWorld )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

visualise :: [Particle] -> Float -> IO ()
visualise ps n = play disp white 60 (WS ps (0,0) False (250,-30) False (250,30) False) toPicture eventHandler changeState
	where
		disp = InWindow "Particles" (1280, 960) (10,10)

		toPicture ws = foldr (\a b -> f a <> b) (border <> slider1Bar <> slider1 <> slider1Text <> slider2Bar <> slider2 <> slider2Text) (particles ws)
			where
				f (Particle _ r (x,y) _ c) = color c $ translate x y $ circleSolid r
				border = line [(-100,-100),(-100,100),(100,100),(100,-100),(-100,-100)]
				slider1Bar = line [(170,-30),(250,-30)]
				slider1 = uncurry translate (slider1Pos ws) (circleSolid 10)
				slider1Text = scale 0.1 0.1 (translate 1200 (-100) (text ("Wall Restitution: " ++ show (((fst $ slider1Pos ws)-170)/80))))
				slider2Bar = line [(170,30),(250,30)]
				slider2 = uncurry translate (slider2Pos ws) (circleSolid 10)
				slider2Text = scale 0.1 0.1 (translate 1200 500 (text ("Particle Restitution: " ++ show (((fst $ slider2Pos ws)-170)/80))))

		changeState t (WS ps (mx,my) clickedPar s1Pos s1Clicked s2Pos s2Clicked) = WS (advanceWorld eWall eParticle (t*n) (f ps clickedPar)) (mx,my) clickedPar newS1Pos s1Clicked newS2Pos s2Clicked
			where
				f [] True = []
				f (p@(Particle _ _ (px,py) _ c):xs) True = if c /= red then p:f xs True else p {velocity = ((mx-px)/((1/10)*n),(my-py)/((1/10)*n))}:xs
				f ps _ = ps
				g (_,s2) True = (max 170 $ min 250 mx,s2)
				g sPos _ = sPos
				newS1Pos = g s1Pos s1Clicked
				eWall = (fst newS1Pos - 170) / 80
				newS2Pos = g s2Pos s2Clicked
				eParticle = (fst newS2Pos - 170) / 80

		eventHandler (EventKey (MouseButton LeftButton) Down _ (mx,my)) ws = ws {mousePos = (mx,my), 
			particles = fst modifiedParticles, clickedParticle = snd modifiedParticles, 
			slider1Clicked = g $ slider1Pos ws, slider2Clicked = g $ slider2Pos ws}
			where
				f [] = ([],False)
				f (p@(Particle _ r (px,py) _ _):xs) = if (mx-px)^2 + (my-py)^2 > r^2 then 
					let (ys,b) = f xs in (p:ys,b) else (p {col = red}:xs,True)
				modifiedParticles = f $ particles ws
				g (sx,sy) = (mx-sx)^2 + (my-sy)^2 <= 10^2
		eventHandler (EventKey (MouseButton LeftButton) Up _ _)  ws = ws {particles = map (\p -> p {col = blue}) (particles ws),
			clickedParticle = False, slider1Clicked = False, slider2Clicked = False}
		eventHandler (EventMotion (mx,my)) ws = ws {mousePos = (mx,my)}
		eventHandler _ ws = ws