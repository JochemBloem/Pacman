module Movable where
    {-
        IMPORTS
     -}
    import Types
    import BFS
    import HelperFunctions
    import Initials
    import RandomNumber
   
    {- 
        SHOW 
     -} 
    instance Show Direction where
        show N = "North"
        show E = "East"
        show S = "South"
        show W = "West"
    instance Show GhostBehaviour where
        show Chase      = "Chase"
        show Scatter    = "Scatter"
        show Frightened = "Frightened"
    instance Show Pacman where
        show (Pacman lo d li _) = "Pacman:\n    Location:  " ++ show lo ++ "\n    Direction: " ++ show d ++ "\n    Lives:     " ++ show li ++ "\n" 
    instance Show Ghost where
        show (Blinky l d g t i finit)  = stringifyGhost "Blinky" l d g t i finit
        show (Pinky  l d g t i finit)  = stringifyGhost "Pinky"  l d g t i finit
        show (Inky   l d g t i finit)  = stringifyGhost "Inky"   l d g t i finit
        show (Clyde  l d g t i finit)  = stringifyGhost "Clyde"  l d g t i finit
    
    stringifyGhost :: String -> Location -> Direction -> GhostBehaviour -> Float -> Int -> Float -> String
    stringifyGhost name loc dir gb ms i finit = name ++ "\n    " ++ "Location:       " ++ show loc ++ "\n    Direction:      " ++ show dir ++ "\n    Behaviour:      " ++ show gb ++ "\n    Internal clock: " ++ show ms ++ "s\n    rndindex:    " ++ show i ++ "Finit: " ++ show finit
        
    {- 
        MOVABLE 
     -}
    class Movable a where 
        changeDirection :: a -> Direction -> Maze -> a
        move            :: a -> Maze      -> a
    
    instance Movable Pacman where 
        changeDirection p@(Pacman lo _ li ma) d m | pacmanAccessible targetField = Pacman lo d li ma
                                                  | otherwise                    = p
                where
                    (_, targetField) = getTargetTile m lo d
        move p@(Pacman lo@(x,y) d li ma) m     | pacmanAccessible targetField = Pacman targetLoc d li newangle'
                                               | otherwise                    = p {location = (round' x, round' y)} -- put pacman in the middle of the field, so he can change direction
            where 
                (targetLoc, targetField)     = getTargetTile m lo d
                newangle                     = ma - 8
                newangle' | newangle < (-45) = 45
                          | otherwise        = newangle
    
    instance Movable Ghost where
        -- change ghost directions
        changeDirection (Blinky l _ g t i finit) d _ = Blinky l d g t i finit
        changeDirection (Pinky  l _ g t i finit) d _ = Pinky  l d g t i finit
        changeDirection (Inky   l _ g t i finit) d _ = Inky   l d g t i finit
        changeDirection (Clyde  l _ g t i finit) d _ = Clyde  l d g t i finit   
        -- move ghosts
        move g@(Blinky l@(x, y) d gb t i finit) m  | ghostAccessible targetField = Blinky targetLoc             d gb t i finit
                                                   | otherwise                   = Blinky (round' x, round' y)  d gb t i finit
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Pinky  l@(x, y) d gb t i finit) m  | ghostAccessible targetField = Pinky   targetLoc            d gb t i finit
                                                   | otherwise                   = Pinky   (round' x, round' y) d gb t i finit
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Inky   l@(x, y) d gb t i finit) m  | ghostAccessible targetField = Inky    targetLoc            d gb t i finit
                                                   | otherwise                   = Inky    (round' x, round' y) d gb t i finit
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Clyde  l@(x, y) d gb t i finit) m  | ghostAccessible targetField = Clyde   targetLoc            d gb t i finit
                                                   | otherwise                   = Clyde   (round' x, round' y) d gb t i finit
            where (targetLoc, targetField) = getTargetTile m l d

    getTargetTile :: Maze -> Location -> Direction -> TargetTile
    getTargetTile m (x,y) N = (newLoc, getField m checkLoc) -- TODO isGhost boolean
        where 
            v               = movableSpeed
            checkLoc        = (round' x,     round' $ y + (0.5 + v))
            newLoc          = (x, y + v)
    getTargetTile m (x,y) E = (newLoc, getField m checkLoc) 
        where 
            v               = movableSpeed
            checkLoc        = (round' $ x + (0.5 + v), round' y)
            newLoc          = (x + v, y)
    getTargetTile m (x,y) S = (newLoc, getField m checkLoc) 
        where 
            v               = movableSpeed
            checkLoc        = (round' x,     round' $ y - (0.5 + v))
            newLoc          = (x, y - v)
    getTargetTile m (x,y) W = (newLoc, getField m checkLoc) 
        where 
            v               = movableSpeed
            checkLoc        = (round' $ x - (0.5 + v), round' y)
            newLoc          = (x - v, y)
    
    shiftLocation :: Direction -> Location -> Float -> Location
    shiftLocation N (x,y) n = (x  , y+n)
    shiftLocation E (x,y) n = (x+n, y  )
    shiftLocation S (x,y) n = (x  , y-n)
    shiftLocation W (x,y) n = (x-n, y  )

    
    
    {- 
        GHOSTS 
     -}

    aiSteps :: Gamestate -> [Ghost] -> [Ghost]
    aiSteps gstate = map (aiStep gstate)
    
    aiStep :: Gamestate -> Ghost -> Ghost
    aiStep gstate g@(Blinky loc dir gb ms i finit) | isEaten pacLoc g                 = baseBlinky ms
                                                   | gb == Frightened && isOnTile loc = randomDirection g m loc
                                                   | isOnTile loc                     = Blinky loc newDir gb ms i finit
                                                   | otherwise                        = g -- Cant change direction
                                                    where 
                                                        pacman   = player gstate
                                                        (px, py) = location pacman
                                                        pacLoc   = (round' px, round' py)
                                                        m        = maze gstate
                                                        newDir   | gb == Chase      = findPath m loc pacLoc dir
                                                                 | gb == Scatter    = findPath m loc (scatterLocation g) dir
    aiStep gstate g@(Pinky  loc dir gb ms i finit) | isEaten pacLoc g                 = basePinky ms
                                                   | gb == Frightened && isOnTile loc = randomDirection g m loc
                                                   | isOnTile loc                     = Pinky loc newDir gb ms i finit
                                                   | otherwise                        = g -- Cant change direction
                                                    where 
                                                        pacman   = player gstate
                                                        (px, py) = location pacman
                                                        pacLoc   = (round' px, round' py)
                                                        m        = maze gstate
                                                        newDir   | gb == Chase      = findPath m loc (shiftLocation (direction pacman) pacLoc 4) dir
                                                                 | gb == Scatter    = findPath m loc (scatterLocation g) dir
    aiStep gstate g@(Inky   loc dir gb ms i finit) | isEaten pacLoc g                 = baseInky ms
                                                   | gb == Frightened && isOnTile loc = randomDirection g m loc
                                                   | isOnTile loc                     = Inky loc newDir gb ms i finit
                                                   | otherwise                        = g -- Cant change direction
                                                    where 
                                                        pacman   = player gstate
                                                        (px, py) = location pacman
                                                        pacLoc   = (round' px, round' py)
                                                        m        = maze gstate
                                                        newDir   | gb == Chase      = findPath m loc inkyChaseLoc dir
                                                                 | gb == Scatter    = findPath m loc (scatterLocation g) dir
                                            
                                                        -- new location aim when chasing
                                                        inkyChaseLoc :: Location
                                                        inkyChaseLoc = (round' x, round' y)
                                                                where 
                                                                    (Blinky (bix,biy) _ _ _ _ _) = head (enemies gstate) -- gstate always contains enemies, if not a crash is fine
                                                                    (px,py) = pacLoc
                                                                    (bx,by) = (round' bix, round' biy)
                                                                    (x,y)   = (bix + (px - bix) * 2,biy + (py - biy) * 2)


    aiStep gstate g@(Clyde  loc dir gb ms i finit) | isEaten pacLoc g                 = baseClyde ms
                                                   | gb == Frightened && isOnTile loc = randomDirection g m loc        
                                                   | isOnTile loc                     = Clyde loc newDir gb ms i finit
                                                   | otherwise                        = g -- Cant change direction
                                                    where 
                                                        pacman   = player gstate
                                                        (px, py) = location pacman
                                                        pacLoc   = (round' px, round' py)
                                                        m        = maze gstate

                                                        newDir   | gb == Chase && distance loc pacLoc > 5 = findPath m loc pacLoc dir     
                                                                 | otherwise                              = findPath m loc (scatterLocation g) dir

    randomDirection :: Ghost -> Maze -> Location -> Ghost
    randomDirection g m loc = changeDirection ng newDir m
                        where
                            -- @TODO check if length nbs < 3, than return g
                            nbs          = accessibleNeighbours m loc
                            (newDir, ng) = pickDirection loc nbs

                            pickDirection :: Location -> [Location] -> (Direction, Ghost)
                            pickDirection current nbs' | intersection = randomNotReverse   g        nbs'
                                                       | corridor     = (getGhostDirection g             , g)
                                                       | corner       = randomNotReverse   g        nbs'
                                                       | deadend      = (getDirection current (head nbs'), g)
                                                       where
                                                        intersection = length nbs' >= 3
                                                        corridor     = length nbs' == 2 && (oppositeDirection (getDirection current (head nbs')) == getDirection current (nbs' !! 1))
                                                        corner       = length nbs' == 2
                                                        deadend      = length nbs' == 1
                            randomNotReverse :: Ghost -> [Location] -> (Direction, Ghost)
                            randomNotReverse g nbs'' =  (chosenDir, ng')
                                             where
                                                currentLoc   = getGhostLocation  g
                                                currentDir   = getGhostDirection g
                                                filtered     = filter (opposite currentDir currentLoc) nbs''
                                                (index, ng') = newRnd g (length filtered - 1)
                                                chosenLoc    = filtered !! index
                                                chosenDir    = getDirection currentLoc chosenLoc

                            opposite :: Direction -> Location -> Location -> Bool
                            opposite curDir curLoc newLoc = getDirection curLoc newLoc /= oppositeDirection curDir



    
    updateGhostTimers :: Float -> [Ghost] -> [Ghost]
    updateGhostTimers s = map (updateGhostTimer s)

    updateGhostTimer :: Float -> Ghost -> Ghost
    updateGhostTimer secs (Blinky l d g t i finit) = Blinky l d g (t + extraTime g secs) i finit
    updateGhostTimer secs (Pinky  l d g t i finit) = Pinky  l d g (t + extraTime g secs) i finit
    updateGhostTimer secs (Inky   l d g t i finit) = Inky   l d g (t + extraTime g secs) i finit
    updateGhostTimer secs (Clyde  l d g t i finit) = Clyde  l d g (t + extraTime g secs) i finit

    extraTime :: GhostBehaviour -> Float -> Float
    extraTime Frightened _ = 0
    extraTime _          s = s

    updateGhostBehaviour:: Ghost -> Float -> Ghost
    updateGhostBehaviour g now | getGhostBehaviour g == Frightened  = unFrighten g now
                               | modded > round' chaseTime          = setGhostBehaviour Scatter g
                               | otherwise                          = setGhostBehaviour Chase   g
                                where
                                    modded = round' (getGhostTime g) `mod` round' (chaseTime + scatterTime) 