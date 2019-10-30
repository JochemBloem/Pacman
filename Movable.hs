module Movable where
    {-
        IMPORTS
     -}
    import Types
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
        changeDirection p@(Pacman lo _ li) d m | targetField == Item = Pacman lo d li
                                               | otherwise           = p
                where
                    (_, targetField) = getTargetTile m lo d
        move p@(Pacman lo@(x,y) d li) m     | targetField == Item = Pacman targetLoc d li
                                            | otherwise           = p {location = (round' x, round' y)} -- put pacman in the middle of the field, so he can change direction
            where 
                (targetLoc, targetField)    = getTargetTile m lo d
    
    instance Movable Ghost where
        -- change ghost directions
        changeDirection (Blinky l _ g t) d _ = Blinky l d g t
        changeDirection (Pinky  l _ g t) d _ = Pinky  l d g t
        changeDirection (Inky   l _ g t) d _ = Inky   l d g t
        changeDirection (Clyde  l _ g t) d _ = Clyde  l d g t
        -- move ghosts
        move g@(Blinky l d gb t) m  | targetField == Wall = g 
                                    | otherwise           = Blinky targetLoc d gb t
            where (targetLoc, targetField)                = getTargetTile m l d
        move g@(Pinky  l d gb t) m  | targetField == Wall = g 
                                    | otherwise           = Pinky  targetLoc d gb t
            where (targetLoc, targetField)                = getTargetTile m l d
        move g@(Inky   l d gb t) m  | targetField == Wall = g 
                                    | otherwise           = Inky   targetLoc d gb t
            where (targetLoc, targetField)                = getTargetTile m l d
        move g@(Clyde  l d gb t) m  | targetField == Wall = g 
                                    | otherwise           = Clyde  targetLoc d gb t
            where (targetLoc, targetField)                = getTargetTile m l d

    getTargetTile :: Maze -> Location -> Direction -> TargetTile
    getTargetTile m (x,y) N = (newLoc, getField m checkLoc) 
        where 
            v               = pacmanSpeed
            checkLoc        = (round' x,     round' $ y + (0.5 + v))
            newLoc          = (x, y + v)
    getTargetTile m (x,y) E = (newLoc, getField m checkLoc) 
        where 
            v               = pacmanSpeed
            checkLoc        = (round' $ x + (0.5 + v), round' y)
            newLoc          = (x + v, y)
    getTargetTile m (x,y) S = (newLoc, getField m checkLoc) 
        where 
            v               = pacmanSpeed
            checkLoc        = (round' x,     round' $ y - (0.5 + v))
            newLoc          = (x, y - v)
    getTargetTile m (x,y) W = (newLoc, getField m checkLoc) 
        where 
            v               = pacmanSpeed
            checkLoc        = (round' $ x - (0.5 + v), round' y)
            newLoc          = (x - v, y)
    
    getField :: Maze -> Location -> Field
    getField m l = m!!locationToIndex l 

    {- 
        PACMAN 
     -}
    initialPacman :: Pacman
    initialPacman = Pacman initialPacmanLocation S 3
    
    initialPacmanLocation :: Location
    initialPacmanLocation = (7,3)

    {- 
        GHOSTS 
     -}

    initialEnemies :: [Ghost]
    initialEnemies = [Blinky (7,10) W Chase 0, Pinky (7,7) S Chase 0, Inky (6,7) N Chase 0, Clyde (8,7) N Chase 0]

    aiSteps :: Gamestate -> [Ghost] -> [Ghost]
    aiSteps gstate= map (aiStep gstate)
    
    aiStep :: Gamestate -> Ghost -> Ghost
    aiStep gstate g@(Blinky loc dir gb ms) | gb == Chase      = g { direction = findPath loc pacLoc }
                                           | gb == Scatter    = g
                                           | gb == Frightened = g
                                           where 
                                            (px, py) = location $ player gstate 
                                            pacLoc   = (round' px, round' py)
    aiStep gstate g@(Pinky  loc dir gb ms) = g
    aiStep gstate g@(Inky   loc dir gb ms) = g
    aiStep gstate g@(Clyde  loc dir gb ms) = g

    findPath :: Location -> Location -> Direction
    findPath l1 l2 = undefined


    findPath' :: Location -> Location -> Queue -> Resilt

    updateGhostTimers :: Float -> [Ghost] -> [Ghost]
    updateGhostTimers s = map (updateGhostTimer s)

    updateGhostTimer :: Float -> Ghost -> Ghost
    updateGhostTimer secs (Blinky l d g t) = Blinky l d g (t + secs)
    updateGhostTimer secs (Pinky  l d g t) = Pinky  l d g (t + secs)
    updateGhostTimer secs (Inky   l d g t) = Inky   l d g (t + secs)
    updateGhostTimer secs (Clyde  l d g t) = Clyde  l d g (t + secs)