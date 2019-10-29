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
    
    stringifyGhost :: String -> Location -> Direction -> GhostBehaviour -> Int -> String
    stringifyGhost name loc dir gb ms = name ++ "\n    " ++ "Location:       " ++ show loc ++ "\n    Direction:      " ++ show dir ++ "\n    Behaviour:      " ++ show gb ++ "\n    Internal clock: " ++ show ms ++ "ms"
        
    {- 
        MOVABLE 
     -}
    class Movable a where 
        changeDirection :: a -> Direction -> a
        move            :: a -> Maze      -> a
    
    instance Movable Pacman where 
        changeDirection (Pacman lo _ li) d  = Pacman lo d li
        move p@(Pacman lo d li) m           | targetField == Item = Pacman targetLoc d li
                                            | otherwise           = p
            where 
                (targetLoc, targetField)    = getTargetTile m lo d
    
    instance Movable Ghost where
        -- change ghost directions
        changeDirection (Blinky l _ g t) d = Blinky l d g t
        changeDirection (Pinky  l _ g t) d = Pinky  l d g t
        changeDirection (Inky   l _ g t) d = Inky   l d g t
        changeDirection (Clyde  l _ g t) d = Clyde  l d g t
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
    getTargetTile m (x,y) N = (newLoc, getField m newLoc) 
        where newLoc        = (x, y - 1)
    getTargetTile m (x,y) E = (newLoc, getField m newLoc) 
        where newLoc        = (x - 1, y)
    getTargetTile m (x,y) S = (newLoc, getField m newLoc) 
        where newLoc        = (x, y + 1)
    getTargetTile m (x,y) W = (newLoc, getField m newLoc) 
        where newLoc        = (x, y - 1)
    
    getField :: Maze -> Location -> Field
    getField m l = m!!locationToIndex l

    {- 
        PACMAN 
     -}


    initialPacman :: Pacman
    initialPacman = Pacman initialPacmanLocation N 3
    
    initialPacmanLocation :: Location
    initialPacmanLocation = (10,10)

    {- 
        GHOSTS 
     -}

    initialEnemies :: [Ghost]
    initialEnemies = [Blinky (1,1) S Chase 0]