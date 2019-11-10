-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Graphics.Gloss.Data.Picture
    import Types
    import Movable
    import HelperFunctions
    import Data.Sort
    
    view :: Gamestate -> IO Picture
    view gstate | status gstate == GameOver =   let (sx, sy)         = screenSizeF
                                                    hwidth           = pixelsPerField / 3
                                                    (middlex', _)    = boardSizeI
                                                    middlex          = fromIntegral $ middlex' `div` 2
                                                    pacman           = viewPacman (Pacman (middlex - 2,0) W 0 45)
                                                    ghosts'          = [           Blinky (middlex - 1,0) W Chase 0 0 0
                                                                       ,           Pinky  (middlex    ,0) W Chase 0 0 0
                                                                       ,           Inky   (middlex + 1,0) W Chase 0 0 0
                                                                       ,           Clyde  (middlex + 2,0) W Chase 0 0 0
                                                                       ]
                                                    ghosts           = map (`viewGhost` gstate) ghosts'
                                                    top              = fst screenSizeF
                                                    up               = 0.5 * top
                                                    characters       = scale' 2 $ Translate hwidth up $ Pictures $ pacman : ghosts
                                                    shiftDown amount = Translate 0 (-amount)
                                                    shiftLeft amount = Translate (-amount) 0 
                                                    centre           = shiftLeft (9*hwidth)
                                                    scoreText        = color white . centre . scale' 0.2
                                                    gameover         = centre . scale' 0.25 . shiftDown (-45*hwidth) . color red $ Text "Game Over!"
                                                    highscore score  =                        scoreText $ Text ("High score: " ++ score)
                                                    yourscore        = shiftDown (5*hwidth) . scoreText $ Text ("Your score: " ++ show (score gstate)) 
                                                in 
                                                  do
                                                    content          <- readFile "scores/highscores.txt"
                                                    let highscores'  = lines content
                                                        highscores   | not (null highscores') = map read highscores' :: [Int]
                                                                     | otherwise              = [0]
                                                        highest      = maximum highscores
                                                    return $ Pictures [gameover, characters, yourscore, highscore $ show highest]

                | otherwise                 = (return . viewPure) gstate

    viewPure :: Gamestate -> Picture
    viewPure gstate = case status gstate of
                      Paused   ->  Pictures [header,body,footer]
                                where 
                                  (sx, sy) = screenSizeF
                                  header   = Scale 0.5 0.5 $ color blue (Translate ((-1)*sx/2) 0 $ Text "PAUSED")
                                  body     = Scale 0.3 0.3 $ color blue (Translate ((-1)*sx) ((-1)*sy/2) $ Text "Press u to unpause")
                                  footer   = Scale 0.2 0.2 $ color blue (Translate ((-1)*sx) ((-1)*sy) $ Text "Or press r to restart")
                      GameOn   -> Pictures (viewMaze (maze gstate) ++ viewGhosts (enemies gstate) gstate ++ [viewPacman $ player gstate] ++ [viewHeaders gstate])
                                  
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
                    fsize    = scaledFieldSize 0.7  / 2

              
    
    {-
        UI view functions
     -}
     
    viewHeaders :: Gamestate -> Picture
    viewHeaders (Gamestate _ (Pacman _ _ lives _) _ _ score level' _ _ _ _) = Translate dx dy $ Pictures [pacmans', shiftRight (20*hwidth) $ headerText points, shiftRight (35*hwidth) $ headerText level]
                    where
                      (x, y)            = screenSizeF
                      dx                = (-1) * (x / 2) * 0.9
                      dy                =        (y / 2) * 0.88
                      shiftRight dist   = Translate dist 0
                      hwidth            = pixelsPerField / 3
                      maxDisplayedLives = min 5 lives
                      overflowLives     = max 0 (lives - maxDisplayedLives)
                      headerPacman      = Translate 0 (hwidth / 2) $ rotate' E $ viewBasePacman hwidth 45
                      pacmans           = zip (replicate maxDisplayedLives headerPacman) [0..]
                      tfwidths n        = Translate (hwidth * 2 * n) 0
                      pacmans'          = Pictures $ [tfwidths n pacman | (pacman, n) <- pacmans] ++ overflow
                      overflow          | overflowLives > 0 = [shiftRight (hwidth * 10) $ scale' 0.1 $ color white $ Text ("+ " ++ show overflowLives)]
                                        | otherwise         = []
                      headerText t      = scale' 0.2 $ color white $ Text t
                      points            = show score ++ " pts"
                      level             = "Level "   ++ show level'
    {-
        Movable view functions
     -}
    viewPacman :: Pacman -> Picture
    viewPacman p@(Pacman loc dir _ ma) = Translate dx dy $ rotate' dir (viewBasePacman hwidth ma)
                                    where 
                                      i        = locationToIndex loc
                                      (dx, dy) = characterSpaceToScreenSpace $ location p
                                      hwidth   = pixelsPerField / 3
                                      angle    = abs ma

    viewBasePacman :: Float -> Float -> Picture
    viewBasePacman hwidth angle = Rotate (-90) $ color (makeColorI 255 255 0 255) $ arcSolid angle (360-angle) hwidth -- rotate so it faces north, so we can use the rotate' function

    viewGhosts :: [Ghost] -> Gamestate -> [Picture]
    viewGhosts gs gstate = map (`viewGhost` gstate) gs

    viewGhost :: Ghost -> Gamestate -> Picture
    viewGhost g@(Blinky loc dir gb _ _ finit) gstate = color c $ ghostPicture loc dir
                                   where
                                    c | gb == Frightened = frightenedColor (levelTimer gstate) finit
                                      | otherwise        = ghostColor g
    viewGhost g@(Pinky  loc dir gb _ _ finit) gstate = color c $ ghostPicture loc dir
                                   where
                                    c | gb == Frightened = frightenedColor (levelTimer gstate) finit
                                      | otherwise        = ghostColor g
    viewGhost g@(Inky   loc dir gb _ _ finit) gstate = color c $ ghostPicture loc dir
                                   where
                                    c | gb == Frightened = frightenedColor (levelTimer gstate) finit
                                      | otherwise        = ghostColor g
    viewGhost g@(Clyde  loc dir gb _ _ finit) gstate = color c $ ghostPicture loc dir
                                   where
                                    c | gb == Frightened = frightenedColor (levelTimer gstate) finit
                                      | otherwise        = ghostColor g

    frightenedColor :: Float -> Float -> Color
    frightenedColor lt finit | difference < blinkingTimer = darkblue
                             | blink                      = white
                             | otherwise                  = darkblue
                              where
                                darkblue   = makeColorI 33  33  255 255
                                white      = makeColorI 255 255 255 255
                                difference = lt - finit
                                blink      = (even . floor) difference

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

                              -- eyes
                              eye1       = Translate (-eyespacing) 0 $ rotate' dir eye
                              eye2       = Translate   eyespacing  0 $ rotate' dir eye
                              
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

    