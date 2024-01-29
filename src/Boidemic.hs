{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# language StrictData #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Boid = Boid deriving Show
instance Component Boid where type Storage Boid = Map Boid

newtype BoidSprite = BoidSprite Picture deriving Show
instance Semigroup BoidSprite where
  BoidSprite Blank <> p2 = p2
  p1 <> BoidSprite Blank = p1
  p1 <> p2 = p1
instance Monoid BoidSprite where mempty = BoidSprite Blank
instance Component BoidSprite where type Storage BoidSprite = Global BoidSprite

newtype PlayerSprite = PlayerSprite Picture deriving Show
instance Semigroup PlayerSprite where
  PlayerSprite Blank <> p2 = p2
  p1 <> PlayerSprite Blank = p1
  p1 <> p2 = p1
instance Monoid PlayerSprite where mempty = PlayerSprite Blank
instance Component PlayerSprite where type Storage PlayerSprite = Global PlayerSprite


newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

newtype MaxSpeed = MaxSpeed Float deriving Show
instance Component MaxSpeed where type Storage MaxSpeed = Map MaxSpeed

newtype Angle = Angle Float deriving Show -- Angle in radians
instance Component Angle where type Storage Angle = Map Angle

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data Obstacle = Obstacle deriving Show
instance Component Obstacle where type Storage Obstacle = Map Obstacle


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

newtype KeySet = KeySet { unKeySet :: S.Set Key } deriving Show
instance Semigroup KeySet where
  ksX <> ksY = KeySet (unKeySet ksX `S.union` unKeySet ksY)
instance Monoid KeySet where mempty = KeySet S.empty
instance Component KeySet where type Storage KeySet = Global KeySet

makeWorld "World"
  [ ''Camera
  , ''Player, ''Boid, ''Obstacle
  , ''BoidSprite, ''PlayerSprite
  , ''Position, ''Velocity, ''MaxSpeed, ''Angle
  , ''KeySet
  ]

type System' a = System World a
type Kinetic = (Position, Velocity, MaxSpeed, Angle)


-------------------------------------------
-- User input
-------------------------------------------

data Action
  = LeftA
  | RightA
  | UpA
  | DownA
  deriving (Show, Eq, Ord, Enum)

type KeyBindings = M.Map Key Action

defaultKeyBindings :: KeyBindings
defaultKeyBindings = M.fromList
  [ (SpecialKey KeyUp, UpA)
  , (SpecialKey KeyLeft, LeftA)
  , (SpecialKey KeyDown, DownA)
  , (SpecialKey KeyRight, RightA)
  , (Char 'w', UpA)
  , (Char 'a', LeftA)
  , (Char 's', DownA)
  , (Char 'd', RightA)
  ]


handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyEsc) Down   _ _) = liftIO exitSuccess
handleEvent (EventKey k Down _ _) = modify global $ \(KeySet ks) -> KeySet (S.insert k ks)
handleEvent (EventKey k Up _ _)   = modify global $ \(KeySet ks) -> KeySet (S.delete k ks)
handleEvent _ = return ()

-------------------------------------------
-- Magic numbers
-------------------------------------------


-- type Kinetic = (Position, Velocity)
-- , ymin, ymax :: Double
areaWidth, areaHeight, boidMaxSpeed, playerMaxSpeed, separationDist, sightRadius, followRadius :: Float
areaWidth = 400
areaHeight = 400
-- xmax = (areaWidth - playerW) / 2 - 5
-- xmin = -xmax
boidMaxSpeed = 100
playerMaxSpeed = 100
sightRadius = 100
followRadius = 300
separationDist = 20

cohesionFactor, separationFactor, alignmentFactor, followFactor :: Float
cohesionFactor = 0.01
separationFactor = 0.8
alignmentFactor = 0.1
followFactor = 0.01


-------------------------------------------
-- Spawning entities
-------------------------------------------

newBoid :: V2 Float -> V2 Float -> System' ()
newBoid pos v
  = newEntity_ (Boid, Obstacle, Position pos, Velocity v, Angle 0, MaxSpeed boidMaxSpeed)

initPlayer :: V2 Float -> System' ()
initPlayer pos
  = newEntity_ (Player, Obstacle, Position pos, Velocity 0, Angle 0, MaxSpeed playerMaxSpeed)

-------------------------------------------
-- Game Logic
-------------------------------------------

-- This page is a particularly useful point of reference: http://www.kfish.org/boids/pseudocode.html

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

stepPosition :: Float -> System' ()
stepPosition dT
  = cmap $ \(Velocity v, Position p) -> Position (p + dT *^ v)

gameLogic :: System' ()
gameLogic = do
  cohesion
  separation
  alignment -- boids zoom off without a speed limit
  followPlayer
  playerVelocity
  speedLimit
  syncAngle

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
  localDists <- collect $ \(Obstacle, Position p')
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

speedLimit :: System' ()
speedLimit
  = cmapIf (\(Velocity v, MaxSpeed s) -> norm v > s)
           (\(Boid, Velocity v, MaxSpeed s) -> Velocity (s *^ signorm v))

syncAngle :: System' ()
syncAngle
  = cmapIf (\(Velocity v) -> v /= 0) -- Prevent div by 0
           (\(Angle _, Velocity v) -> Angle (unangle v))

playerVelocity :: System' ()
playerVelocity = do
  KeySet ks <- get global
  let actions = S.foldr' (\k acc -> maybe acc -- If key not in bindings, don't insert
                                          (`S.insert` acc)
                                          (defaultKeyBindings M.!? k))
                         S.empty
                         ks

      vs = S.toList actions
            |> map actionToVelocity
            |> sum

  cmap $ \(Player, Velocity v) -> Velocity (playerMaxSpeed *^ vs)

actionToVelocity :: Action -> V2 Float
actionToVelocity = \case
  UpA    -> V2  0  1
  LeftA  -> V2 -1  0
  DownA  -> V2  0 -1
  RightA -> V2  1  0

followPlayer :: System' ()
followPlayer
  = cmapM_ $ \(Player, Position t) ->
      cmapIf 
        (\(Position p) -> withinRange followRadius t p) -- Only follow player if in sight
        (\(Boid, Position p, Velocity v) ->
            let dv = followFactor *^ (t - p)
            in Velocity (v + dv))


-------------------------------------------
-- Rest
-------------------------------------------

translate' :: V2 Float -> Picture -> Picture
translate' (V2 x y) = translate x y

rotate' :: Float -> Picture -> Picture
rotate' rads = rotate (rads * -radToDegreeFactor)

radToDegreeFactor :: Float
radToDegreeFactor = 180 / pi

draw :: System' Picture
draw = do
  BoidSprite boidSprite <- get global
  PlayerSprite playerSprite <- get global
  boids <- foldDraw $ \(Boid, Position pos, Angle a) ->
                translate' pos $ rotate' a boidSprite
  player <- foldDraw $ \(Player, Position pos, Angle a) ->
                translate' pos $ rotate' a playerSprite
  
  pure $ boids <> player

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
  initPlayer (V2 0 0)


boidemicMain :: IO ()
boidemicMain = do
  w <- initWorld
  boidSprite <- loadBMP "src/boid.bmp"
  playerSprite <- loadBMP "src/player.bmp"
  setStdGen (mkStdGen 0)
  runWith w $ do
    set global (BoidSprite boidSprite)
    set global (PlayerSprite playerSprite)
    initialise
    play display
         black
         60
         draw
         handleEvent
         step

-------------------------------------------
-- Utility
-------------------------------------------

logDebug :: String -> System' ()
-- logDebug = liftIO . putStrLn
logDebug _ = pure ()

toRadians :: Floating a => a -> a
toRadians x = x * (pi / 180)

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal x = min maxVal (max minVal x)

toMaybe :: Bool -> a -> Maybe a
toMaybe q x = if q then Just x else Nothing

withinRange :: Float -> V2 Float -> V2 Float -> Bool
withinRange radius p1 p2 = distance p1 p2 <= radius
