{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# language StrictData #-}
module Boidemic (boidemicMain) where

import Apecs
import Apecs.Physics.Gloss
import Apecs.Gloss

import Linear ( V2(..), angle, (*^), distance, sumV, (^/), unangle, signorm, norm )

import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Witch
import Flow
import Data.String.Interpolate (i, __i)
import Data.Fixed (mod')
import Apecs.System (cmapIf)

data Boid = Boid deriving Show
instance Component Boid where type Storage Boid = Map Boid

newtype BoidSprite = BoidSprite Picture deriving Show
instance Semigroup BoidSprite where
  BoidSprite Blank <> p2 = p2
  p1 <> BoidSprite Blank = p1
  p1 <> p2 = p1
instance Monoid BoidSprite where mempty = BoidSprite Blank
instance Component BoidSprite where type Storage BoidSprite = Global BoidSprite

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

-- newtype Angle = Angle Float deriving Show -- Angle in radians
-- instance Component Angle where type Storage Angle = Map Angle

-- newtype Speed = Speed Float deriving Show
-- instance Component Speed where type Storage Speed = Map Speed


-- data Player = Player deriving Show
-- instance Component Player where type Storage Player = Unique Player

-- newtype Score = Score Int deriving (Show, Num)
-- instance Semigroup Score where (<>) = (+)
-- instance Monoid Score where mempty = 0
-- instance Component Score where type Storage Score = Global Score

-- newtype Time = Time Float deriving (Show, Num)
-- instance Semigroup Time where (<>) = (+)
-- instance Monoid Time where mempty = 0
-- instance Component Time where type Storage Time = Global Time

-- newtype LeftDown = LeftDown Bool deriving Show
-- instance Component LeftDown where type Storage LeftDown = Global LeftDown
-- instance Semigroup LeftDown where LeftDown p <> LeftDown q = LeftDown (p && q)
-- instance Monoid LeftDown where mempty = LeftDown False

-- newtype RightDown = RightDown Bool deriving Show
-- instance Component RightDown where type Storage RightDown = Global RightDown
-- instance Semigroup RightDown where RightDown p <> RightDown q = RightDown (p && q)
-- instance Monoid RightDown where mempty = RightDown False

makeWorld "World" [''Camera, ''Boid, ''BoidSprite, ''Position, ''Velocity]

type System' a = System World a
type Kinetic = (Position, Velocity)


-- type Kinetic = (Position, Velocity)
-- , ymin, ymax :: Double
areaWidth, areaHeight, maxSpeed, separationDist :: Float
areaWidth = 400
areaHeight = 400
-- xmax = (areaWidth - playerW) / 2 - 5
-- xmin = -xmax
maxSpeed = 100
sightRadius = 100
separationDist = 30

cohesionFactor, separationFactor, alignmentFactor :: Float
cohesionFactor = 0.02
separationFactor = 0.1
alignmentFactor = 0.1

newBoid :: V2 Float -> V2 Float -> System' ()
newBoid pos v
  = newEntity_ (Boid, Position pos, Velocity v)





logDebug :: String -> System' ()
-- logDebug = liftIO . putStrLn
logDebug _ = pure ()

toRadians :: Floating a => a -> a
toRadians x = x * (pi / 180)

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal x = min maxVal (max minVal x)

-- Particle effects systems

stepPosition :: Float -> System' ()
stepPosition dT
  = cmap $ \(Velocity v, Position p) -> Position (p + dT *^ v)

-- spawnParticles :: Int -> V2 Float -> (Float, Float) -> (Float, Float) -> System' ()
-- spawnParticles n pos dvx dvy = replicateM_ n $ do
--   vx <- liftIO $ randomRIO dvx
--   vy <- liftIO $ randomRIO dvy
--   t  <- liftIO $ randomRIO (0.02,0.3)
--   particle <- newEntity (Particle t, Position pos, Velocity (V2 vx vy))
--   logDebug [__i|New particle: #{particle}|]

-- setPlayerVelocity :: System' ()
-- setPlayerVelocity = cmap $ \(Player, LeftDown ld, RightDown rd) -> 
--   if |     ld && not rd -> Velocity (V2 -playerSpeed 0)
--      | not ld &&     rd -> Velocity (V2  playerSpeed 0)
--      | otherwise        -> Velocity (V2 0 0)

step :: Float -> System' ()
step dT = do
  gameLogic
  -- incrTime dT
  stepPosition dT
  -- setPlayerVelocity
  -- clampPlayer
  -- stepParticles dT
  -- stepPhysics (1/60)
  -- stepPosition dT
  -- clampPlayer
  -- clearBlocks
  -- clearBullets
  -- stepParticles dT
  -- handleCollisions

-------------------------------------------
-- Game Logic
-------------------------------------------

-- This page is a particularly useful point of reference: http://www.kfish.org/boids/pseudocode.html

gameLogic :: System' ()
gameLogic = do
  cohesion
  separation
  alignment -- boids are zooming off. need a speed limit
  speedLimit

cohesion :: System' ()
cohesion = cmapM $ \(Boid, Position p, Velocity v) -> do
  localPs <- collect $ \(Boid, Position p') -> toMaybe (withinRange sightRadius p p') p'
  -- let centreMass = sumV localPs ^/ (fromIntegral $ length localPs)
  let v' = case localPs of
            []  -> v
            [_] -> v -- Only local boid is the one we want to update
            _   -> 
              let centreMass = (sumV localPs - p)
                            ^/ (fromIntegral $ length localPs - 1)
                  dv = cohesionFactor *^ (centreMass - p)
              in  v + dv
  pure $ Velocity v'

separation :: System' ()
separation = cmapM $ \(Boid, Position p, Velocity v) -> do
  localDists <- collect $ \(Boid, Position p')
                            -> toMaybe (withinRange separationDist p p')
                                       (p' - p)
  pure $ Velocity (v - separationFactor *^ sumV localDists)

alignment :: System' ()
alignment = cmapM $ \(Boid, Position p, Velocity v) -> do
  localVs <- collect $ \(Boid, Position p', Velocity v')
                        -> toMaybe (withinRange separationDist p p') v'

  let v' = case localVs of
            []  -> v
            [_] -> v -- Only local boid is the one we want to update
            _   -> 
              let meanV = (sumV localVs - v) ^/ (fromIntegral $ length localVs - 1)
                  dv = alignmentFactor *^ meanV
              in  v + dv
  pure $ Velocity v'

withinRange :: Float -> V2 Float -> V2 Float -> Bool
withinRange radius p1 p2 = distance p1 p2 <= radius

speedLimit :: System' ()
speedLimit
  = cmapIf (\(Velocity v) -> norm v > maxSpeed)
           (\(Boid, Velocity v) -> Velocity (maxSpeed *^ signorm v))

toMaybe :: Bool -> a -> Maybe a
toMaybe q x = if q then Just x else Nothing

-------------------------------------------
-- Rest
-------------------------------------------

handleEvent :: Event -> System' ()
-- handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
--   set global (LeftDown True)

-- handleEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) =
--   set global (LeftDown False)

-- handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
--   set global (RightDown True)

-- handleEvent (EventKey (SpecialKey KeyRight) Up   _ _) =
--   set global (RightDown False)

-- -- handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
-- --   cmapM_ $ \(Player, pos) -> do
-- --     _ <- newEntity (Ball, pos, Velocity (V2 0 ballSpeed))
-- --     spawnParticles 7 pos (-80,80) (10,100)

handleEvent (EventKey (SpecialKey KeyEsc) Down   _ _) = liftIO exitSuccess

handleEvent _ = return ()

translate' :: V2 Float -> Picture -> Picture
translate' (V2 x y) = translate x y

rotate' :: Float -> Picture -> Picture
rotate' rads = rotate (rads * -radToDegreeFactor)

radToDegreeFactor :: Float
radToDegreeFactor = 180 / pi

draw :: System' Picture
draw = do
  foldDrawM $ \(Boid, Position pos, Velocity v) -> do
                 BoidSprite sprite <- get global
                 pure (translate' pos $ rotate' (unangle v) sprite)

display :: Display
display = InWindow "Boidemic" (640, 640) (10, 10)

randomSpawnBoids :: Int -> System' ()
randomSpawnBoids n = replicateM_ n $ do
  pos <- liftIO $ randomRIO (-50, 50)
  v   <- liftIO $ randomRIO (-30, 30)
  newBoid pos v

initialise :: System' ()
initialise = do
  set global ( Camera 0 1 )
  randomSpawnBoids 5


boidemicMain :: IO ()
boidemicMain = do
  w <- initWorld
  boidSprite <- loadBMP "src/boid.bmp"
  setStdGen (mkStdGen 0)
  runWith w $ do
    set global (BoidSprite boidSprite)
    initialise
    play display
         black
         60
         draw
         handleEvent
         step

