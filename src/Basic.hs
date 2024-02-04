{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
module Basic (basicMain) where

import Apecs
import Apecs.Gloss

newtype Position = Pos (Float, Float)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Vel (Float, Float)
instance Component Velocity where type Storage Velocity = Map Velocity

makeWorld "Model" [''Camera, ''Position, ''Velocity ]

type System' a = System Model a

stepPos :: Float -> System' ()
stepPos dt
  = cmap $ \(Pos (x,y), Vel (vX, vY))
            -> Pos (x + vX * dt, y + vY * dt)

basicMain :: IO ()
basicMain = do
  w <- initModel
  runWith w $ do
    set global ( Camera 0 1 )
    newEntity_ (Pos (0,0), Vel (10,10))
    play display
         background
         60
         view
         inputController
         stepController
  where
    display = InWindow "Basic" (1024, 1024) (10, 10)
    background = black
    inputController _ = pure ()

view :: System' Picture
view = foldDraw $ \(Pos (x,y)) -> translate x y $ color red $ circle 10

stepController :: Float -> System' ()
stepController dt = stepPos dt