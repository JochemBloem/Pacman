module Movable where
    {-
        IMPORTS
     -}
    import Types
    import BFS
    import HelperFunctions

   
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
        show (Pacman lo d li ) = "Pacman:\n    Location:  " ++ show lo ++ "\n    Direction: " ++ show d ++ "\n    Lives:     " ++ show li ++ "\n" 
    instance Show Ghost where
        show (Blinky l d g t)  = stringifyGhost "Blinky" l d g t
        show (Pinky  l d g t)  = stringifyGhost "Pinky"  l d g t
        show (Inky   l d g t)  = stringifyGhost "Inky"   l d g t
        show (Clyde  l d g t)  = stringifyGhost "Clyde"  l d g t
    
    stringifyGhost :: String -> Location -> Direction -> GhostBehaviour -> Float -> String
    stringifyGhost name loc dir gb ms = name ++ "\n    " ++ "Location:       " ++ show loc ++ "\n    Direction:      " ++ show dir ++ "\n    Behaviour:      " ++ show gb ++ "\n    Internal clock: " ++ show ms ++ "ms"
        
    {- 
        MOVABLE 
     -}
    class Movable a where 
        changeDirection :: a -> Direction -> Maze -> a
        move            :: a -> Maze      -> a
    
    instance Movable Pacman where 
        changeDirection p@(Pacman lo _ li) d m | pacmanAccessible targetField = Pacman lo d li
                                               | otherwise                    = p
                where
                    (_, targetField) = getTargetTile m lo d
        move p@(Pacman lo@(x,y) d li) m     | pacmanAccessible targetField = Pacman targetLoc d li
                                            | otherwise                    = p {location = (round' x, round' y)} -- put pacman in the middle of the field, so he can change direction
            where 
                (targetLoc, targetField)    = getTargetTile m lo d
    
    instance Movable Ghost where
        -- change ghost directions
        changeDirection (Blinky l _ g t) d _ = Blinky l d g t
        changeDirection (Pinky  l _ g t) d _ = Pinky  l d g t
        changeDirection (Inky   l _ g t) d _ = Inky   l d g t
        changeDirection (Clyde  l _ g t) d _ = Clyde  l d g t
        -- move ghosts
        move g@(Blinky l@(x, y) d gb t) m  | ghostAccessible targetField = Blinky targetLoc             d gb t
                                           | otherwise                   = Blinky (round' x, round' y)  d gb t
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Pinky  l@(x, y) d gb t) m  | ghostAccessible targetField = Pinky  targetLoc            d gb t
                                           | otherwise                   = Pinky  (round' x, round' y) d gb t
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Inky   l@(x, y) d gb t) m  | ghostAccessible targetField = Inky   targetLoc            d gb t
                                           | otherwise                   = Inky   (round' x, round' y) d gb t
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Clyde  l@(x, y) d gb t) m  | ghostAccessible targetField = Clyde  targetLoc            d gb t
                                           | otherwise                   = Clyde  (round' x, round' y) d gb t
            where (targetLoc, targetField) = getTargetTile m l d

    getTargetTile :: Maze -> Location -> Direction -> TargetTile
    getTargetTile m (x,y) N = (newLoc, getField m checkLoc) 
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


    
    
    {- 
        GHOSTS 
     -}

    aiSteps :: Gamestate -> [Ghost] -> [Ghost]
    aiSteps gstate = map (aiStep gstate)
    
    aiStep :: Gamestate -> Ghost -> Ghost
    aiStep gstate g@(Blinky loc dir gb ms) | isOnTile loc = Blinky loc newDir gb ms
                                           | otherwise    = g -- Cant change direction
                                           where 
                                            (px, py) = location $ player gstate 
                                            pacLoc   = (round' px, round' py)
                                            m        = maze gstate
                                            newDir   | gb == Chase      = findPath m loc pacLoc
                                                     | gb == Scatter    = dir -- @TODO
                                                     | gb == Frightened = randomDirection m loc
    aiStep gstate g@(Pinky  loc dir gb ms) = g
    aiStep gstate g@(Inky   loc dir gb ms) = g
    aiStep gstate g@(Clyde  loc dir gb ms) = g

    randomDirection :: Maze -> Location -> Direction
    randomDirection m loc = getDirection loc newLoc
                        where
                            nbs    = accessibleNeighbours m loc
                            newLoc = nbs!!getRandomNumber 0 (length nbs)
    
    updateGhostTimers :: Float -> [Ghost] -> [Ghost]
    updateGhostTimers s = map (updateGhostTimer s)

    updateGhostTimer :: Float -> Ghost -> Ghost
    updateGhostTimer secs (Blinky l d g t) = Blinky l d g (t + secs)
    updateGhostTimer secs (Pinky  l d g t) = Pinky  l d g (t + secs)
    updateGhostTimer secs (Inky   l d g t) = Inky   l d g (t + secs)
    updateGhostTimer secs (Clyde  l d g t) = Clyde  l d g (t + secs)