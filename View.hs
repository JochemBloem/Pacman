-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Graphics.Gloss.Data.Picture
    import Types
    import Movable
    import HelperFunctions
    
    view :: Gamestate -> IO Picture
    view  = return . viewPure

    viewPure :: Gamestate -> Picture
    viewPure gstate = case status gstate of
                      Paused   -> Translate ((-1)*sx/2) 0 $ Scale 0.1 0.1 $ color blue $ Text $ show $ player gstate {-color blue (Translate ((-1)*sx/2) 0 $ Text "PAUSED")-}
                                where 
                                  (sx, sy) = screenSizeF
                      GameOn   -> Pictures (viewMaze (maze gstate) ++ viewGhosts (enemies gstate) ++ [viewPacman $ player gstate] ++ [viewHeaders gstate])
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
    viewField (Empty,     i) = Blank
    viewField (Energizer, i) = color yellow (Translate dx dy $ circleSolid (scaledFieldSize 0.3  / 2))
                  where 
                    (dx, dy) = toScreenSpace i
    viewField (Dot,       i) = color yellow (Translate dx dy $ circleSolid (scaledFieldSize 0.15 / 2))
                  where 
                    (dx, dy) = toScreenSpace i
    viewField (Fruit,     i) = color red (Translate dx dy $ rectangleWire fsize fsize)
                  where 
                    (dx, dy) = toScreenSpace i
                    fsize = scaledFieldSize 0.7  / 2

              
    
    {-
        UI view functions
     -}
     
    viewHeaders :: Gamestate -> Picture
    viewHeaders (Gamestate _ (Pacman _ _ lives _) ( Blinky _ _ _ s :_ ) _ score level _ _) = Translate dx dy $ scale' 0.2 $ color white $ Text headers
                    where
                      (x, y) = screenSizeF
                      dx = (-1) * (x / 2) * 0.9
                      dy =        (y / 2) * 0.88
                      headers = "Lives: " ++ show lives ++ "  Score: " ++ show score ++ "  Level: " ++ show level ++ "  Bclock: " ++ show s
    {-
        Movable view functions
     -}
    viewPacman :: Pacman -> Picture
    viewPacman p@(Pacman loc dir _ ma) = Translate dx dy $ rotate' dir basePacman -- @TODO Mouth opening and closing animation: use variable arc in basePacman. 
                                    where 
                                      i        = locationToIndex loc
                                      (dx, dy) = characterSpaceToScreenSpace $ location p
                                      hwidth   = pixelsPerField / 3
                                      angle    = abs ma

                                      basePacman :: Picture
                                      basePacman = Rotate (-90) $ color (makeColorI 255 255 0 255) $ arcSolid angle (360-angle) hwidth -- rotate so it faces north, so we can use the rotate' function

    viewGhosts :: [Ghost] -> [Picture]
    viewGhosts = map viewGhost

    viewGhost :: Ghost -> Picture
    viewGhost (Blinky loc dir _ _) = color (makeColorI 255 0   0   255) $ ghostPicture loc dir
    viewGhost (Pinky  loc dir _ _) = color (makeColorI 255 184 255 255) $ ghostPicture loc dir
    viewGhost (Inky   loc dir _ _) = color (makeColorI 0   255 255 255) $ ghostPicture loc dir
    viewGhost (Clyde  loc dir _ _) = color (makeColorI 255 184 82  255) $ ghostPicture loc dir

    ghostPicture :: Location -> Direction -> Picture
    ghostPicture loc dir = Translate dx dy $ Pictures [ arcSolid 0 180 (fieldSize / 3), polygon path, eye1, eye2]
                            where
                              i          = locationToIndex loc
                              (dx, dy)   = characterSpaceToScreenSpace loc
                              path       = [(-hwidth,0),(hwidth,0),(hwidth,tbottom), (u2,ttop),(0, tbottom),(u1,ttop),(-hwidth, tbottom)]
                              hwidth     = pixelsPerField / 3
                              tbottom    = -hwidth
                              ttop       = tbottom + 0.5*hwidth
                              u1         = 0.33 * hwidth
                              u2         = 2*u1
                              eyespacing = 0.5*hwidth
                              eye1       = Translate (-eyespacing) 0 $ rotate' dir eye
                              eye2       = Translate   eyespacing  0 $ rotate' dir eye

                              -- eyes
                              
                              eye :: Picture
                              eye = Pictures [ color white $ circleSolid eyesize, Translate 0 (eyesize - pupilsize) $ color black $ circleSolid pupilsize ]
                                  where
                                    eyesize   = 0.3  * hwidth
                                    pupilsize = 0.15 * hwidth
                              
    rotate' :: Direction -> Picture -> Picture
    rotate' N p = p
    rotate' E p = Rotate 90  p
    rotate' S p = Rotate 180 p
    rotate' W p = Rotate 270 p
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

    scale' :: Float -> Picture -> Picture
    scale' f = Scale f f

    