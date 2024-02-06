{-# language TypeFamilies #-}
{-# language TemplateHaskell #-}
module Basic (basicMain) where

import Apecs
import Apecs.Gloss
import System.Exit (exitSuccess)

-------------------------------------------
-- Components
-------------------------------------------

newtype Position = Pos (Float, Float)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Vel (Float, Float)
instance Component Velocity where type Storage Velocity = Map Velocity

-------------------------------------------
-- Model
-------------------------------------------

makeWorld "Model" [''Camera, ''Position, ''Velocity ]

type System' a = System Model a

-------------------------------------------
-- View
-------------------------------------------

view :: System' Picture
view
  = foldDraw (\(Pos (x,y))
               -> translate x y (circle 10))

-------------------------------------------
-- Controller
-------------------------------------------

inputController :: Event -> System' ()
inputController event = case event of
  EventKey (SpecialKey KeyEsc) Down _ _
    -> liftIO exitSuccess
  _ -> pure ()

stepPos :: Float -> System' ()
stepPos dt
  = cmap (\(Pos (x,y), Vel (vX, vY))
           -> Pos (x + vX * dt, y + vY * dt))

-------------------------------------------
-- Spawn entities
-------------------------------------------

createEntities :: System' ()
createEntities = do
  newEntity_ (Pos (0,0), Vel (10,10))
  newEntity_ (Pos (50,50), Vel (-10,-10))

-------------------------------------------
-- Run game
-------------------------------------------

game :: System' ()
game = do
  set global ( Camera 0 5 )
  createEntities
  play display
       white -- background colour
       60    -- FPS
       view
       inputController
       stepPos
  where
    display = InWindow "Basic" (1024, 1024) (10, 10)

basicMain :: IO ()
basicMain = do
  w <- initModel
  runWith w game
