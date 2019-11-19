--Criteria For imporved asteroids

-- 1)Create an UFO datatype.

-- 2)Add the UFO to the game state.

-- 3)Adjust the initialWorld function 

--   so that the UFO is initialized properly.

-- 4)Adjust the drawWorld function so that the

--   UFO is drawn.

-- 5)djust the simulateWorld function so that

--   the UFO moves around. 






-- Criteria for TypeClass2

-- The student has to Create a typeclass called Spatial 

-- that each of the aforementioned types implements

-- 1) Rocks and bullets,

-- 2) ufo and bullets, 

-- 3) ship and rocks.



--{-# LANGUAGE DeriveAnyClass #-}

module EXFancy where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import Graphics.Gloss.Interface.Pure.Simulate

import Graphics.Gloss.Interface.Pure.Display



data AsteroidWorld = Play [Rock] Ship [Bullet] UFO      --Note01: The UFO has been added to the initial state

                   | GameOver | Winning

                   deriving (Eq,Show)



type Velocity     = (Float, Float)

type Size         = Float

type Age          = Float





--The status of fleening it means that it flys in fast speed (not implemented)

--The hunted means it is hited by a few bullest.(not implemented)

--The exploding 

data UfoState= Hunting| Fleeing| Exploding   deriving (Eq,Show)

                                                 ---The fllowing UFO data type has four compnent  

data UFO = UFO {location :: PointInSpace,        -- The locaton for its postion               

                speed    :: Velocity,            -- The vilocity

                state    :: UfoState,            -- State which could be one of three state.

                health   :: Int,                 -- health whcih track the health of ufo and check how many time has been shoted

                size     :: Float,                -- To give dimension for the UFO

                clr    :: Color}deriving (Eq,Show) --Color it temperory I will cancel it latter.



data Ship   = Ship PointInSpace Velocity      

    deriving (Eq,Show)

data Bullet = Bullet PointInSpace Velocity Age  

    deriving (Eq,Show)

data Rock   = Rock PointInSpace Size Velocity 

    deriving (Eq,Show)





class Spatial a where 

     pInSpace  :: a-> PointInSpace   -- This will return the point in space for any object 

     sizeOfTheObject :: a -> Float



instance Spatial UFO where

     pInSpace (UFO ufoP v st h s c)=ufoP

     sizeOfTheObject (UFO ufoP v st h s c) = s



instance Spatial Ship where

     pInSpace (Ship p v)= p 

     sizeOfTheObject(Ship p v)= 10



instance Spatial Bullet where

     pInSpace (Bullet p _  _)= p

     sizeOfTheObject (Bullet p _  _)=4



instance Spatial Rock where

     pInSpace (Rock p _  _)= p

     sizeOfTheObject (Rock _ s  _)= s





collides:: (Spatial a, Spatial b) => a->b->Bool

collides x y= let

              dl= magV $((.-) (pInSpace x) (pInSpace y))

              dsize=0.5 * abs (sizeOfTheObject x + sizeOfTheObject y)

              in dl < dsize



initialWorld :: AsteroidWorld

initialWorld = Play

                   [Rock (150,150)  45 (2,6)    

                   ,Rock (-45,201)  45 (13,-8) 

                   ,Rock (45,22)    25 (-2,8)  

                   ,Rock (-210,-15) 30 (-2,-8) 

                   ,Rock (-45,-201) 25 (8,2)   

                   ] -- The default rocks

                   (Ship (0,0) (0,5)) -- The initial ship

                   [] -- The initial bullets (none)

                   (UFO (120,120) (-20,20) Fleeing 5  20 green)          --Note03:The initialWorld function adjusted so that the UFO is initialized properly





simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)



simulateWorld _        GameOver          = GameOver

simulateWorld _        Winning          = Winning

  



simulateWorld timeStep w@(Play rocks (Ship shipPos shipV) bullets ufo  ) 

  | any (collides (Ship shipPos shipV)) rocks = GameOver

  | collides (Ship shipPos shipV) ufo= GameOver   -- When the space ship collide with ufo the game is over.

  | any (collides  ufo) bullets = if (health ufo)==1

                                  then Winning

                                  else Play (concatMap updateRock rocks) 

                                            (Ship newShipPos shipV)

                                            (concat (map updateBullet bullets))  (updateUFO2  (updateUFOStatus ufo) w ) -- If you hit the ufo you win

  | otherwise = Play (concatMap updateRock rocks) 

                              (Ship newShipPos shipV)

                              (concat (map updateBullet bullets))  (updateUFO2  ufo w ) --Note07 Here simulation so that the UFO moves around

  where

      updateUFO2 :: UFO -> AsteroidWorld -> UFO

      updateUFO2 r@(UFO p v st h s c) aw =if (astrologer aw timeStep)

                                          then UFO (restoreToScreen (p .+ timeStep .* v .+ (3.1,3.0))) ((-1).*v) st h s c  -- Note06:UFO moves around

                                          else UFO (restoreToScreen (p .+ timeStep .* v)) v st h s c  -- Note06:UFO moves around

      updateUFO :: UFO -> UFO

      updateUFO r@(UFO p v st h s c) = UFO (restoreToScreen (p .+ timeStep .* v)) v st h s c  -- Note06:UFO moves around

     

      updateUFOStatus :: UFO -> UFO

      updateUFOStatus g@(UFO p v st h s c)

            |h>=4=UFO p v st (h-1) s c 

            |h>=3 && h<4 = UFO p v Hunting (h-1) s yellow

            |h>=2 && h<3 = UFO p v Exploding (h-1) (s*1.5) red   



  

      updateRock :: Rock -> [Rock]

      updateRock r@(Rock p s v) 

       | (any ( collides  r) bullets )  && s < 7 

            = []

       | (any ( collides  r) bullets ) && s > 7 

            = splitRock r

       | otherwise                     

            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]

 

      updateBullet :: Bullet -> [Bullet] 

      updateBullet bullet@(Bullet p v a) 

        | a > 5                      

             = []

        | any (collides bullet) rocks 

             = [] 

        | any (collides bullet) [ufo]   -- When the bullet hit the UfO it disapear

             =[]

        | otherwise                  

             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 

                       (a + timeStep)] 



      newShipPos :: PointInSpace

      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)



 

--------------------------------------------------------	  

--------------------------------------------------------	  

--------------------------------------------------------	  

simulateWorldUFO timeStep w@(Play rocks (Ship shipPos shipV) bullets ufo  ) 

  

  | any (collides  ufo) bullets = if (health ufo)==1

                                  then Winning

                                  else Play  rocks 

                                            (Ship newShipPos shipV)

                                            (concat (map updateBullet bullets))  (updateUFO (updateUFOStatus ufo)) -- If you hit the ufo you win

  | otherwise = Play          rocks 

                              (Ship newShipPos shipV)

                              (concat (map updateBullet bullets))  (updateUFO  ufo) --Note07 Here simulation so that the UFO moves around

  where

      

      updateUFO :: UFO -> UFO

      updateUFO r@(UFO p v st h s c) = UFO (restoreToScreen (p .+ timeStep .* v)) v st h s c  -- Note06:UFO moves around

     

      updateUFOStatus :: UFO -> UFO

      updateUFOStatus g@(UFO p v st h s c)

            |h>=4=UFO p v st (h-1) s c 

            |h>=3 && h<4 = UFO p v Hunting (h-1) s yellow

            |h>=2 && h<3 = UFO p v Exploding (h-1) (s*1.5) red   



    

      updateBullet :: Bullet -> [Bullet] 

      updateBullet bullet@(Bullet p v a) 

        | a > 5                      

             = []

        | any (collides bullet) rocks 

             = [] 

        | any (collides bullet) [ufo]   -- When the bullet hit the UfO it disapear

             =[]

        | otherwise                  

             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 

                       (a + timeStep)] 



      newShipPos :: PointInSpace

      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)

--------------------------------------------------------	  

--------------------------------------------------------	  

--------------------------------------------------------	  

 

splitRock :: Rock -> [Rock]

splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)

                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]



restoreToScreen :: PointInSpace -> PointInSpace

restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)



cycleCoordinates :: (Ord a, Num a) => a -> a

cycleCoordinates x 

    | x < (-400) = 800+x

    | x > 400    = x-800

    | otherwise  = x





drawWorld :: AsteroidWorld -> Picture 



drawWorld astroidW

    | astroidW ==GameOver = Pictures [scale 0.3 0.3 

                            . translate (-400) 0 

                            . color green 

                            . text 

                            $"Game Over", 



                            scale 0.2 0.2 

                            . translate (-600) (-150) 

                            . color green 

                            $ text "Press ' r ' to restart"] 

    | astroidW ==Winning =  Pictures [scale 0.3 0.3 

                            . translate (-400) 0 

                            . color green 

                            . text 

                            $"You win the Game", 



                            scale 0.2 0.2 

                            . translate (-600) (-150) 

                            . color green 

                            $ text "Press ' r ' If you Want play new One"] 

-- Note08: Modified the Game Over screen 







drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets (UFO (ux,uy) v _ _ z c))

  = pictures [ship, asteroids,shots, ufo]

   where 

    ship      = color red (pictures [translate x y (circle 10)])

-- Note04:I have changed the shape of the rock I use Thick Circle for that.

--  And I make new color I use add color (yellow red and black) 

-- so the resulted color is brown not orange

    asteroids = pictures [translate x y (color  (addColors  yellow (addColors black red))  (ThickCircle 1 s)) 

                         | Rock   (x,y) s _ <- rocks]

    shots     = pictures [translate x y (color red (circle 2)) 

                         | Bullet (x,y) _ _ <- bullets]

    ufo      = color c (pictures [translate ux uy (arcSolid 0 180 z)])--color green  (pictures [translate ux uy (circle z)]) -- Note05: This part responsible to draw UFO in circle shape in green color

                                                                     -- "translate ux uy" part responsible of the movment of UFO in the screen accoriding to

                                                                     -- its position and its speed.



handleEvents :: Event -> AsteroidWorld -> AsteroidWorld



handleEvents (EventKey (Char 'r') Down _ _) (GameOver) = initialWorld

handleEvents (EventKey (Char 'r') Down _ _) (Winning) = initialWorld



-- //////////

--Note10:handleEvents _ GameOver = initialWorld    

-- Here If we activate the above line.  

--The game initial when some one touch the mouse or keyboard

-- ///////



-- Note09:I made so, that the Game Over screen stays on

-- until r button is pressed.

-- The button can be changed for mouse too.

-- But thought it's better to be something

-- specific then just anything



-- The following event handler to responsible to restart the 

-- game when we press the "r" key



handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)

             (Play rocks (Ship shipPos shipVel) bullets ufo)

             = Play rocks (Ship shipPos newVel) 

                          (newBullet : bullets) ufo

 where 

     newBullet = Bullet shipPos 

                        (-150 .* norm (shipPos .- clickPos)) 

                        0

     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))



handleEvents _ w = w



type PointInSpace = (Float, Float)

(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace

(x,y) .- (u,v) = (x-u,y-v)

(x,y) .+ (u,v) = (x+u,y+v)



(.*) :: Float -> PointInSpace -> PointInSpace

s .* (u,v) = (s*u,s*v)



infixl 6 .- , .+

infixl 7 .*



norm :: PointInSpace -> PointInSpace

norm (x,y) = let m = magV (x,y) in (x/m,y/m)



magV :: PointInSpace -> Float

magV (x,y) = sqrt (x**2 + y**2) 



limitMag :: Float -> PointInSpace -> PointInSpace

limitMag n pt = if (magV pt > n) 

                  then n .* (norm pt)

                  else pt



rotateV :: Float -> PointInSpace -> PointInSpace

rotateV r (x,y) = (x * cos r - y * sin r

                   ,x * sin r + y * cos r)



-- The following function will tell me  if the UFO destroyed. 

astrologer :: AsteroidWorld-> Float ->Bool

astrologer Winning _= True

astrologer p@(Play rocks (Ship shipPos shipV) bullets ufo) t = any (==Winning)$ take 10 $ iterate ((\t1 gs -> simulateWorldUFO (t1+0.1) gs) t) p

astrologer _ _= False







main = play 

         (InWindow "AstroidFancyUFO!" (550,550) (30,30)) 

         white 

         24 

         initialWorld 

         drawWorld 

         handleEvents

         simulateWorld

 