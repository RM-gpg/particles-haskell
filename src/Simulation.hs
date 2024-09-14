module Simulation where

import Types
import Control.Monad.Tardis
import Graphics.Gloss.Data.Color

{- Advances the particles by the specified time. -}
advanceWorld :: Float -> Float -> Float -> [Particle] -> [Particle]
advanceWorld eWall eParticle t ps = advanceWorld' t ps 0
	where
	advanceWorld' 0 ps _ = map zeroVel (adjustOverlapping eParticle ps [])
	advanceWorld' _ ps 100 = map zeroVel (adjustOverlapping eParticle ps [])
	advanceWorld' t ps n = advanceWorld' (t-bw) (map zeroVel ps') (n+1)
		where
			(ps',(bw,_)) = runTardis (adjustParticles eWall eParticle ps) (t,(t,0))


{- Sets the x,y velocities of a particle to 0 if it is below a threshold. -}
zeroVel :: Particle -> Particle
zeroVel p@(Particle _ _ _ (vx,vy) _) = p {velocity = (if abs vx < 0.01 then 0 else vx, if abs vy < 0.01 then 0 else vy)}


{- Pushes apart overlapping particles in a list. -}
adjustOverlapping :: Float -> [Particle] -> [Particle] -> [Particle]
adjustOverlapping _ [] acc = acc
adjustOverlapping e (x:xs) acc = let (x',xs') = adjustOverlappingParticle e x xs [] in adjustOverlapping e xs' (x':acc)

{- Pushes apart particles in a list from a single paricle, and that single particle from all particles in the list. -}
adjustOverlappingParticle :: Float -> Particle -> [Particle] -> [Particle] -> (Particle,[Particle])
adjustOverlappingParticle _ p [] acc = (p,acc)
adjustOverlappingParticle e p (x:xs) acc = let (p',x') = overlapping e p x in adjustOverlappingParticle e p' xs (x':acc)

{- Checks if two particles are overlapping and pushes them apart if they are. -}
overlapping :: Float -> Particle -> Particle -> (Particle,Particle)
overlapping e p1@(Particle _ r1 (px1,py1) (vx1,vy1) _) p2@(Particle _ r2 (px2,py2) (vx2,vy2) _)
	| (px1-px2)^2+(py1-py2)^2 >= (r1+r2)^2 || (px1 == px2 && py1 == py2)  = (p1,p2)
	| (vx1 == vx2 && vy1 == vy2) || theta >= pi/2 = (p1 {position = (px1',py1')},p2 {position = (px2',py2')})
	| otherwise = let (p1',p2') = collide e 0 p1 p2 in (p1' {position = (px1',py1')},p2' {position = (px2',py2')})
	where
       	        (x,y) = (px1+(px2-px1)*r1/(r1+r2),py1+(py2-py1)*r1/(r1+r2))
                helper a b
             	        | a >= 0 = acos (b/sqrt (a^2+b^2))
                       	| otherwise = pi + acos (-b/sqrt (a^2+b^2))
                phi = helper (px1-x) (py1-y)
		theta = helper (vx2-vx1) (vy2-vy1) - phi
		phi' = phi+pi
		px1' = limiter (100-r1) (px1 + sin phi * ((r1-sqrt ((x-px1)^2+(y-py1)^2))))
		py1' = limiter (100-r1) (py1 + cos phi * ((r1-sqrt ((x-px1)^2+(y-py1)^2))))
		px2' = limiter (100-r2) (px2 + sin phi' * ((r2-sqrt ((x-px2)^2+(y-py2)^2))))
		py2' = limiter (100-r2) (py2 + cos phi' * ((r2-sqrt ((x-px2)^2+(y-py2)^2))))


{- Computes the next state following the immediate collision of one or two particles if there
 is one in the specified time. -}
adjustParticles :: Float -> Float -> [Particle] -> Tardis Float (Float,Int) [Particle]
adjustParticles _  _ [] = pure []
adjustParticles eWall eParticle (x:xs) = do
	bw <- getFuture
	(t,n) <- getPast
	let (l,(ctime,_)) = runTardis (replace eParticle x xs) (t,(t,0))
	let (ctimeWall,dir) = collisionTimeWall x
	modifyForwards (\(t,n) -> (minimum [t,ctime,ctimeWall], if ctime == bw || ctimeWall == bw then n+1 else n))
	modifyBackwards (\bw -> minimum [bw,t,ctime,ctimeWall])
	rest <- adjustParticles eWall eParticle xs
	pure $ if ctime == bw && n == 0 then l
		else if ctimeWall == bw && n == 0 then collideWall eWall (ctimeWall,dir) x:rest
		else moveParticle bw x:rest

{- For a single particle, determines the first particle it will collide with in the list
 if there is one in the specified time and calculates the result of the collision. -}
replace :: Float -> Particle -> [Particle] -> Tardis Float (Float,Int) [Particle]
replace _ p [] = do
	(t,n) <- getPast
	pure $ if n == 0 then [moveParticle t p] else []
replace e p (x:xs) = case (collisionTime p x) of
	Nothing -> do
		bw <- getFuture
		(moveParticle bw x:) <$> replace e p xs
	Just ctime -> do
		bw <- getFuture
		(t,n) <- getPast
		modifyForwards (\(t,n) -> (min t ctime, if ctime == bw then n+1 else n))
		modifyBackwards (\bw -> minimum [bw,t,ctime])
		rest <- replace e p xs
		pure $ if ctime == bw && n == 0 then p1 : p2 : rest else moveParticle bw x : rest
			where
				(p1,p2) = collide e ctime p x

{- Moves a single particle by a specified time assuming no collision. -}
moveParticle :: Float -> Particle -> Particle
moveParticle t p@(Particle _ r (px,py) (vx,vy) _) = p {position = (limiter (100-r) (px+t*vx),limiter (100-r) (py+t*vy))}


{- Calculates the minimum time needed for a particle to collide with any wall. -}
collisionTimeWall :: Particle -> (Float,Bool)
collisionTimeWall (Particle _ r (px,py) (vx,vy) _)
	| vx == 0 && vy == 0 = (9999999999,True)
	| vx == 0 && vy > 0 = ((100-py-r)/vy,True)
	| vx == 0 && vy < 0 = ((-100-py+r)/vy,True)
	| vx > 0 && vy == 0 = ((100-px-r)/vx,False)
	| vx > 0 && vy > 0 = if (100-px-r)/vx < (100-py-r)/vy then ((100-px-r)/vx,False) else ((100-py-r)/vy,True)
	| vx > 0 && vy < 0 = if (100-px-r)/vx < (-100-py+r)/vy then ((100-px-r)/vx,False) else ((-100-py+r)/vy,True)
	| vx < 0 && vy == 0 = ((-100-px+r)/vx,False)
	| vx < 0 && vy > 0 = if (-100-px+r)/vx < (100-py-r)/vy then ((-100-px+r)/vx,False) else ((100-py-r)/vy,True)
	| otherwise = if (-100-px+r)/vx < (-100-py+r)/vy then ((-100-px+r)/vx,False) else ((-100-py+r)/vy,True)

{- Calculates the new position and velocity of a particle immediately following a collision with a wall. -}
collideWall :: Float -> (Float,Bool) -> Particle -> Particle
collideWall e (t,vert) p@(Particle _ _ (px,py) (vx,vy) _)
	| vert = p {position = (px+vx*t,py+vy*t), velocity = (vx,-e*vy)}
	| otherwise = p {position = (px+vx*t,py+vy*t), velocity = (-e*vx,vy)}


{- Calculates the minimum time needed for two particles to collide, returns Nothing if they never collide.
 Solution to 't' from this equation (time when centers of particles are separated by their radii): 
 ((px1+t*vx1)-(px2+t*vx2))^2 + ((py1+t*vy1)-(py2+t*vy2))^2 = (r1+r2)^2 -}
collisionTime :: Particle -> Particle -> Maybe Float
collisionTime (Particle _ r1 (px1,py1) (vx1,vy1) _) (Particle _ r2 (px2,py2) (vx2,vy2) _) = 
	if a+b == 0 || srt < 0 || eq < 0 || eq == 0 && (v == 0 || abs theta >= pi/2) then Nothing else Just eq
        where
                a = (vx1-vx2)^2
                b = (vy1-vy2)^2
                c = (px1-px2)^2
                d = (py1-py2)^2
                e = px1*vx1
                f = px1*vx2
                g = vx1*px2
                h = px2*vx2
                i = py1*vy1
                j = py1*vy2
                k = vy1*py2
                l = py2*vy2
                m = e-f-g+h+i-j-k+l
		srt = ((2*m)^2)-4*(a+b)*(c+d-(r1+r2)^2)
                eq = (-(1/2)*sqrt srt -m)/(a+b)

		(x1,y1) = (px1+eq*vx1,py1+eq*vy1)
                (x2,y2) = (px2+eq*vx2,py2+eq*vy2) 
                (x,y) = (x1+(x2-x1)*r1/(r1+r2),y1+(y2-y1)*r1/(r1+r2)) 
                (vx,vy) = (vx2-vx1,vy2-vy1) 
                v = sqrt $ vx^2+vy^2
                helper a b
                        | a >= 0 = acos (b/sqrt (a^2+b^2))
                        | otherwise = pi + acos (-b/sqrt (a^2+b^2))
                phi = helper (x-x2) (y-y2)
                theta = helper vx vy - phi

{- Calculates the new position and velocity of two particles immediately following a collision. -}
collide :: Float -> Float -> Particle -> Particle -> (Particle,Particle)
collide e t p1@(Particle m1 r1 (px1,py1) (vx1,vy1) _) p2@(Particle m2 r2 (px2,py2) (vx2,vy2) _)
        = (p1 {position = (x1,y1), velocity = (vx1'',vy1'')},p2 {position = (x2,y2), velocity = (vx2'',vy2'')})
        where
                (x1,y1) = (px1+t*vx1,py1+t*vy1) -- center of first particle
                (x2,y2) = (px2+t*vx2,py2+t*vy2) -- center of second particle
                (x,y) = (x1+(x2-x1)*r1/(r1+r2),y1+(y2-y1)*r1/(r1+r2)) -- intersection point
                (vx,vy) = (vx2-vx1,vy2-vy1) -- velocity of second particle with respect to first
                v = sqrt $ vx^2+vy^2
                helper a b
                        | a >= 0 = acos (b/sqrt (a^2+b^2))
                        | otherwise = pi + acos (-b/sqrt (a^2+b^2))
                phi = helper (x-x2) (y-y2)
                theta = helper vx vy - phi
                velX = v*cos theta
		velY = v*sin theta
		vx2' = (m2*velX-m1*velX*e)/(m1+m2)
		vx1' = (m2*velX-m2*vx2')/m1
                vx1'' = vx1 + vx1'*sin phi
                vy1'' = vy1 + vx1'*cos phi
                vx2'' = vx1 + vx2'*sin phi + velY*sin (phi+pi/2)
                vy2'' = vy1 + vx2'*cos phi + velY*cos (phi+pi/2)


{- Limits the absolute value of a Float. -}
limiter :: Float -> Float -> Float
limiter lim x = if abs x > lim then signum x * lim else x
