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
        show (Blinky l d g t i)  = stringifyGhost "Blinky" l d g t i
        show (Pinky  l d g t i)  = stringifyGhost "Pinky"  l d g t i
        show (Inky   l d g t i)  = stringifyGhost "Inky"   l d g t i
        show (Clyde  l d g t i)  = stringifyGhost "Clyde"  l d g t i
    
    stringifyGhost :: String -> Location -> Direction -> GhostBehaviour -> Float -> Int -> String
    stringifyGhost name loc dir gb ms i = name ++ "\n    " ++ "Location:       " ++ show loc ++ "\n    Direction:      " ++ show dir ++ "\n    Behaviour:      " ++ show gb ++ "\n    Internal clock: " ++ show ms ++ "s\n    rndindex:    " ++ show i
        
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
        changeDirection (Blinky l _ g t i) d _ = Blinky l d g t i
        changeDirection (Pinky  l _ g t i) d _ = Pinky  l d g t i
        changeDirection (Inky   l _ g t i) d _ = Inky   l d g t i
        changeDirection (Clyde  l _ g t i) d _ = Clyde  l d g t i
        -- move ghosts
        move g@(Blinky l@(x, y) d gb t i) m  | ghostAccessible targetField = Blinky targetLoc             d gb t i
                                             | otherwise                   = Blinky (round' x, round' y)  d gb t i
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Pinky  l@(x, y) d gb t i) m  | ghostAccessible targetField = Pinky  targetLoc            d gb t i
                                             | otherwise                   = Pinky  (round' x, round' y) d gb t i
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Inky   l@(x, y) d gb t i) m  | ghostAccessible targetField = Inky   targetLoc            d gb t i
                                             | otherwise                   = Inky   (round' x, round' y) d gb t i
            where (targetLoc, targetField) = getTargetTile m l d
        move g@(Clyde  l@(x, y) d gb t i) m  | ghostAccessible targetField = Clyde  targetLoc            d gb t i
                                             | otherwise                   = Clyde  (round' x, round' y) d gb t i
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
    aiStep gstate g@(Blinky loc dir gb ms i)| isEaten pacLoc g                 = baseBlinky ms
                                            | gb == Frightened && isOnTile loc = randomDirection g m loc
                                            | isOnTile loc                     = Blinky loc newDir gb ms i
                                            | otherwise                        = g -- Cant change direction
                                           where 
                                            pacman   = player gstate
                                            (px, py) = location pacman
                                            pacLoc   = (round' px, round' py)
                                            m        = maze gstate
                                            newDir   | gb == Chase      = findPath m loc pacLoc dir
                                                     | gb == Scatter    = findPath m loc (scatterLocation g) dir
    aiStep gstate g@(Pinky  loc dir gb ms i)| isEaten pacLoc g                 = basePinky ms
                                            | gb == Frightened && isOnTile loc = randomDirection g m loc
                                            | isOnTile loc                     = Pinky loc newDir gb ms i
                                            | otherwise                        = g -- Cant change direction
                                           where 
                                            pacman   = player gstate
                                            (px, py) = location pacman
                                            pacLoc   = (round' px, round' py)
                                            m        = maze gstate
                                            newDir   | gb == Chase      = findPath m loc (shiftLocation (direction pacman) pacLoc 4) dir
                                                     | gb == Scatter    = findPath m loc (scatterLocation g) dir
    aiStep gstate g@(Inky   loc dir gb ms i)| isEaten pacLoc g                 = baseInky ms
                                            | gb == Frightened && isOnTile loc = randomDirection g m loc
                                            | isOnTile loc                     = Inky loc newDir gb ms i
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
                                                        (Blinky (bix,biy) _ _ _ _) = head (enemies gstate) -- gstate always contains enemies, if not a crash is fine
                                                        (px,py) = pacLoc
                                                        (bx,by) = (round' bix, round' biy)
                                                        (x,y)   = (bix + (px - bix) * 2,biy + (py - biy) * 2)


    aiStep gstate g@(Clyde  loc dir gb ms i)| isEaten pacLoc g                 = baseClyde ms
                                            | gb == Frightened && isOnTile loc = randomDirection g m loc        
                                            | isOnTile loc                     = Clyde loc newDir gb ms i
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
                            -- @TODO check if l;ength nbs < 3, than return g
                            nbs         = accessibleNeighbours m loc
                            (index, ng) = newRnd g (length nbs - 1)
                            newLoc      = nbs!!index
                            newDir      = getDirection loc newLoc



    
    updateGhostTimers :: Float -> [Ghost] -> [Ghost]
    updateGhostTimers s = map (updateGhostTimer s)

    updateGhostTimer :: Float -> Ghost -> Ghost
    updateGhostTimer secs (Blinky l d g t i) = Blinky l d g (t + extraTime g secs) i
    updateGhostTimer secs (Pinky  l d g t i) = Pinky  l d g (t + extraTime g secs) i
    updateGhostTimer secs (Inky   l d g t i) = Inky   l d g (t + extraTime g secs) i
    updateGhostTimer secs (Clyde  l d g t i) = Clyde  l d g (t + extraTime g secs) i

    extraTime :: GhostBehaviour -> Float -> Float
    extraTime Frightened _ = 0
    extraTime _          s = s

    updateGhostBehaviour:: Ghost -> Ghost
    updateGhostBehaviour g | getGhostBehaviour g == Frightened  = g
                           | modded > round' chaseTime          = setGhostBehaviour Scatter g
                           | otherwise                          = setGhostBehaviour Chase   g
                where
                    modded = round' (getGhostTime g) `mod` round' (chaseTime + scatterTime) 