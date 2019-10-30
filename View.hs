-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Types
    import Movable
    
    view :: Gamestate -> IO Picture
    view  = return . viewPure

    viewPure :: Gamestate -> Picture
    viewPure gstate = case status gstate of
                      Paused   -> Translate ((-1)*sx/2) 0 $ Scale 0.1 0.1 $ color blue $ Text $ show $ player gstate {-color blue (Translate ((-1)*sx/2) 0 $ Text "PAUSED")-}
                                where 
                                  (sx, sy) = screenSizeF
                      GameOn   -> Pictures (viewMaze (maze gstate) ++ viewGhosts (enemies gstate) ++ [viewPacman $ player gstate] )
                      GameOver -> Scale 0.5 0.5 $ color red (Translate ((-1)*sx/2) 0 $ Text "GAME OVER")
                                where 
                                  (sx, sy) = screenSizeF
                      
    {-
        Maze view functions
     -}
    viewMaze :: Maze -> [Picture]
    viewMaze m = zipWith (curry viewField) m [0..]

    viewField :: (Field, Int) -> Picture
    viewField (Wall,      i) = color blue (Translate dx dy $ rectangleWire fieldSize fieldSize )
                  where 
                    (dx, dy) = toScreenSpace i
    viewField (Spawn,     i) = Blank
    viewField (SpawnDoor, i) = color red (Line [(dx-w, dy), (dx+w,dy)])
                  where 
                    (dx, dy) = toScreenSpace i
                    w        = fieldSize / 2
    viewField (Item,      i) = color yellow (Translate dx dy $ ThickCircle (scaledFieldSize 0.15 / 2) 1)
                  where 
                    (dx, dy) = toScreenSpace i
    {-
        Movable view functions
     -}
    viewPacman :: Pacman -> Picture
    viewPacman p@(Pacman loc dir _)= Translate dx dy $ color (makeColorI 255 255 0 255) $ Circle (fieldSize / 2) -- TODO: not circle
                                    where 
                                      i        = locationToIndex loc
                                      (dx, dy) = characterSpaceToScreenSpace $ location p

    viewGhosts :: [Ghost] -> [Picture]
    viewGhosts = map viewGhost

    viewGhost :: Ghost -> Picture
    viewGhost (Blinky loc dir _ _) = color (makeColorI 255 0   0   255) $ ghostPicture loc dir
    viewGhost (Pinky  loc dir _ _) = color (makeColorI 255 184 255 255) $ ghostPicture loc dir
    viewGhost (Inky   loc dir _ _) = color (makeColorI 0   255 255 255) $ ghostPicture loc dir
    viewGhost (Clyde  loc dir _ _) = color (makeColorI 255 184 82  255) $ ghostPicture loc dir

    ghostPicture :: Location -> Direction -> Picture
    ghostPicture loc dir = Translate dx dy $ Circle (fieldSize / 3) -- TODO GHOST
                            where
                              i        = locationToIndex loc
                              (dx, dy) = toScreenSpace i
    {-
        Helper functions
     -}
    fieldSize :: Float 
    fieldSize = scaledFieldSize 1

    scaledFieldSize :: Float -> Float
    scaledFieldSize s = pixelsPerField * s

    toScreenSpace :: Int -> (Float, Float)
    toScreenSpace i = (dx, dy)
                    where
                      (width,height) = boardSizeF
                      w2             = max width height / 2
                      d              = fieldSize
                      (x,y)          = indexToLocation i
                      dx             = x * d - w2 * d
                      dy             = y * d - w2 * d

    characterSpaceToScreenSpace :: Location -> Location
    characterSpaceToScreenSpace (x, y) = (nx, ny)
                                          where
                                            (bsizex, bsizey) = boardSizeF
                                            ppf              = pixelsPerField
                                            nx               = ppf*x - (bsizex * ppf / 2)
                                            ny               = ppf*y - (bsizey * ppf / 2)

    