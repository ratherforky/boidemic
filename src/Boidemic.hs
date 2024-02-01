{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# language StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Boidemic (boidemicMain) where

import Apecs
import Apecs.Physics.Gloss
import Apecs.Gloss
import Graphics.Gloss.Juicy


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
import Data.List (sortOn)

data Boid = Boid deriving Show
instance Component Boid where type Storage Boid = Map Boid

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

data Human = Human deriving Show
instance Component Human where type Storage Human = Map Human

newtype HP = HP Int deriving (Show, Eq, Ord)
instance Component HP where type Storage HP = Map HP

newtype HitRadius = HitRadius Float deriving (Show, Eq, Ord)
instance Component HitRadius where type Storage HitRadius = Map HitRadius

data Chip = Chip deriving Show
instance Component Chip where type Storage Chip = Map Chip

data Chips = Chips deriving Show
instance Component Chips where type Storage Chips = Map Chips

data HasChips = HasChips deriving Show
instance Component HasChips where type Storage HasChips = Map HasChips

data BoidState
  = Gliding
  | Attacking Entity
  deriving (Show, Eq)
instance Component BoidState where type Storage BoidState = Map BoidState


-- Sprite with precedence level (lower = further towards back)
data Sprite = Sprite Picture Int deriving Show
instance Component Sprite where type Storage Sprite = Map Sprite

data SpriteStore = SpriteStore
  { boidSprite   :: Picture
  , playerSprite :: Picture
  , humanSprite  :: Picture
  , chipSprite   :: Picture
  , chipsSprite  :: Picture
  } deriving (Show, Eq)

instance Semigroup SpriteStore where
  (<>) = error "only mempty needs to be used for SpriteStore"
instance Monoid SpriteStore where mempty = SpriteStore Blank Blank Blank Blank Blank
instance Component SpriteStore where type Storage SpriteStore = Global SpriteStore

data SyncAngle = SyncAngle deriving Show
instance Component SyncAngle where type Storage SyncAngle = Map SyncAngle


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
  , ''Player, ''Boid, ''Human, ''Chip, ''Chips
  , ''Obstacle
  , ''SpriteStore, ''Sprite
  , ''Position, ''Velocity, ''MaxSpeed, ''Angle, ''SyncAngle
  , ''HitRadius, ''HP, ''BoidState, ''HasChips
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
areaWidth, areaHeight, boidMaxSpeed, playerMaxSpeed, chipSpeed, chipDeceleration, separationDist, sightRadius, followRadius :: Float
areaWidth = 400
areaHeight = 400
-- xmax = (areaWidth - playerW) / 2 - 5
-- xmin = -xmax
boidMaxSpeed = 150
playerMaxSpeed = 200
humanMaxSpeed = 50
chipSpeed = 500
chipDeceleration = 0.03
sightRadius = 50
followRadius = 300
separationDist = 20

cohesionFactor, separationFactor, alignmentFactor, followFactor :: Float
cohesionFactor = 0.01
separationFactor = 0.5
alignmentFactor = 0.05
followFactor = 0.01


-------------------------------------------
-- Spawning entities
-------------------------------------------

randomSpawnBoids :: Int -> System' ()
randomSpawnBoids n = replicateM_ n $ do
  pos <- liftIO $ randomRIO (-50, 50)
  v   <- liftIO $ randomRIO (-30, 30)
  newBoid pos v

newBoid :: V2 Float -> V2 Float -> System' ()
newBoid pos v = do
  SpriteStore{ boidSprite } <- get global
  newEntity_
    ( (Boid, Gliding)
    , Obstacle
    , Position pos
    , Velocity v
    , (Angle 0, SyncAngle)
    , MaxSpeed boidMaxSpeed
    , Sprite boidSprite 10
    , HitRadius 15 
    -- , Gliding -- Instance tuple size goes up to 8, have to make subtuples
    )

initPlayer :: V2 Float -> System' ()
initPlayer pos = do
  SpriteStore{ playerSprite } <- get global
  newEntity_
    ( Player
    , Obstacle
    , Position pos
    , Velocity 0
    , (Angle 0, SyncAngle)
    , MaxSpeed playerMaxSpeed
    , Sprite playerSprite 15
    )

newHuman :: V2 Float -> V2 Float -> System' ()
newHuman pos v = do
  SpriteStore{ humanSprite } <- get global
  newEntity_
    ( Human
    , Position pos
    , Velocity v
    , (Angle 0, SyncAngle)
    , MaxSpeed humanMaxSpeed
    , Sprite humanSprite 5
    , (HitRadius 20, HP 10, HasChips)
    )

spawnChip :: V2 Float -> System' ()
spawnChip pos = do
  SpriteStore{ chipSprite } <- get global
  direction <- liftIO $ randomRIO (0, 2 * pi)
  newEntity_
    ( Chip
    , Position pos
    , Velocity (chipSpeed *^ angle direction)
    , (Angle 0, SyncAngle)
    , MaxSpeed chipSpeed
    , Sprite chipSprite 6
    )

spawnChips :: V2 Float -> System' ()
spawnChips pos = do
  SpriteStore{ chipsSprite } <- get global
  logDebug "Spawn chips"
  direction <- liftIO $ randomRIO (0, 2 * pi)
  newEntity_
    ( Chips
    , Chip -- Just doing this for the deceleration, might cause problems
    , Position pos
    , Velocity (chipSpeed *^ angle direction)
    , Angle 0
    , MaxSpeed chipSpeed
    , Sprite chipsSprite 7
    )


-------------------------------------------
-- Game Logic
-------------------------------------------

-- This page is a particularly useful point of reference: http://www.kfish.org/boids/pseudocode.html

step :: Float -> System' ()
step dT = do
  gameLogic
  -- incrTime dT
  stepPosition dT
  cameraOnPlayer
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
  decelerateChips
  collision
  dropChips

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
           (\(SyncAngle, Angle _, Velocity v) -> Angle (unangle v))

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

cameraOnPlayer :: System' ()
cameraOnPlayer
  = cmap $ \(Player, Position p, Camera _ scale) -> Camera p scale

followPlayer :: System' ()
followPlayer
  = cmapM_ $ \(Player, Position t) ->
      cmapIf 
        (\(Position p) -> withinRange followRadius t p) -- Only follow player if in sight
        (\(Boid, Position p, Velocity v) ->
            let dv = followFactor *^ (t - p)
            in Velocity (v + dv))

collision :: System' ()
collision =
  cmapM_ $ \(Boid, Position posB, HitRadius rB, boidState :: BoidState, etyB :: Entity) ->
    cmapM_ $ \(Human, Position posH, HitRadius rH, HP hp, HasChips, etyH :: Entity) -> do
      case (withinRange (rH + rB) posH posB, boidState) of
        (True, Gliding) -> do
          set etyB (Attacking etyH)
          set etyH (HP $ max 0 (hp - 1))
          logDebug ("New HP: " ++ show (max 0 (hp - 1)))
          spawnChip posH
        (False, Attacking etyH') -> when (etyH == etyH') $ set etyB Gliding
        _ -> pure ()

decelerateChips :: System' ()
decelerateChips = do
  cmap $ \(Chip, Velocity v) ->
            if norm v <= 0.1
              then Velocity 0
              else Velocity ((1 - chipDeceleration) *^ v)

dropChips :: System' ()
dropChips
  = cmapM $ \(Human, HasChips, HP hp, Position p) -> 
      if (hp <= 0)
        then do 
          spawnChips p
          pure $ Right (Not @HasChips) -- Removes the `HasChips` component from the entity
        else pure $ Left () -- NOOP



-------------------------------------------
-- Rest
-------------------------------------------

translate' :: V2 Float -> Picture -> Picture
translate' (V2 x y) = translate x y

rotate' :: Float -> Picture -> Picture
rotate' rads = rotate (rads * -radToDegreeFactor)

radToDegreeFactor :: Float
radToDegreeFactor = 180 / pi

draw :: Picture -> System' Picture
draw background = do
  spritePrecs <- collect $ \(Sprite sprite precedence, Position pos, Angle a) ->
                    Just (precedence, translate' pos $ rotate' a sprite)
  let sprites = sortOn fst spritePrecs |> map snd |> mconcat

  hitcircles <- foldDraw $ \(HitRadius r, Position p) ->
                              translate' p $ color orange $ circle r

  pure $ background <> sprites <> hitcircles

display :: Display
display = InWindow "Boidemic" (1024, 1024) (10, 10)

initialise :: System' ()
initialise = do
  initSpriteStore
  set global ( Camera 0 1 )
  randomSpawnBoids 20
  initPlayer (V2 0 0)
  newHuman (V2 0 50) (V2 0 -10)

initSpriteStore :: System' ()
initSpriteStore = liftIO loadSpriteStore >>= set global

loadSpriteStore :: IO SpriteStore
loadSpriteStore = do
  Just boidBMP   <- loadJuicyPNG "src/seagull.png"
  Just playerBMP <- loadJuicyPNG "src/crow.png"
  Just humanBMP  <- loadJuicyPNG "src/human1.png"
  Just chipBMP   <- loadJuicyPNG "src/chip.png"
  Just chipsBMP  <- loadJuicyPNG "src/chips.png"

  pure $ SpriteStore
    { boidSprite   = rotate 90 $ scale 0.2 0.2 boidBMP
    , playerSprite = rotate 90 $ scale 0.15 0.15 playerBMP
    , humanSprite  = scale 1.3 1.3 humanBMP
    , chipSprite   = scale 0.4 0.4 chipBMP
    , chipsSprite  = scale 0.4 0.4 chipsBMP
    }


boidemicMain :: IO ()
boidemicMain = do
  w <- initWorld
  Just background <- loadJuicyPNG "src/beach-background.png"
  setStdGen (mkStdGen 0)
  runWith w $ do
    initialise
    play display
         green
         60
         (draw $ scale 2 2 background)
         handleEvent
         step

-------------------------------------------
-- Utility
-------------------------------------------

logDebug :: String -> System' ()
logDebug = liftIO . putStrLn
-- logDebug _ = pure ()

toRadians :: Floating a => a -> a
toRadians x = x * (pi / 180)

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal x = min maxVal (max minVal x)

toMaybe :: Bool -> a -> Maybe a
toMaybe q x = if q then Just x else Nothing

withinRange :: Float -> V2 Float -> V2 Float -> Bool
withinRange radius p1 p2 = distance p1 p2 <= radius
