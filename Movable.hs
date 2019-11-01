module Movable where
    {-
        IMPORTS
     -}
    import Types
    import Queue

    import Data.List (sortBy)
    import Data.Function (on)
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

    pacmanAccessible :: Field -> Bool
    pacmanAccessible Item = True
    pacmanAccessible _    = False
    {- 
        GHOSTS 
     -}

    initialEnemies :: [Ghost]
    initialEnemies = [Blinky (7,10) W Chase 0, Pinky (7,7) S Chase 0, Inky (6,7) N Chase 0, Clyde (8,7) N Chase 0]

    aiSteps :: Gamestate -> [Ghost] -> [Ghost]
    aiSteps gstate = map (aiStep gstate)
    
    aiStep :: Gamestate -> Ghost -> Ghost
    aiStep gstate g@(Blinky loc dir gb ms) = Blinky loc newDir gb ms
                                           where 
                                            (px, py) = location $ player gstate 
                                            pacLoc   = (round' px, round' py)
                                            m        = maze gstate
                                            newDir   | gb == Chase      = findPath m loc pacLoc
                                                     | gb == Scatter    = dir
                                                     | gb == Frightened = randomDirection m loc dir
    aiStep gstate g@(Pinky  loc dir gb ms) = g
    aiStep gstate g@(Inky   loc dir gb ms) = g
    aiStep gstate g@(Clyde  loc dir gb ms) = g


    -- @TODO: put these things in another file

    -- Implements the BFS Pathfinding algorithm in Haskell
    findPath :: Maze -> Location -> Location -> Direction
    findPath m l1 l2 = findPath' m l1 l2 (enqueue l1 EmptyQ) []


    findPath' :: Maze -> Location -> Location -> Queue Location -> [(Location, Location)] -> Direction
    findPath' m origin dest q disc | continue && not (discovered disc l) = isdest dest l                    -- this field has not yet been checked
                                   | continue                            = findPath' m origin dest q'' disc' -- this field has         been checked
                                   | otherwise                           = rwalk start disc                 -- q is empty, but the destination has not been found
                         where 
                            (ml, q') = dequeue q
                            (continue, l) = case ml of 
                                        Just l  -> (True,  l     )
                                        Nothing -> (False, defLoc)
                                        
                            an    = accessibleNeighbours m l
                            an'   = filter (not . discovered disc) an
                            
                            -- add the to discovered list
                            disc' = [(n,l) | n <- an'] ++ disc
                            -- enqueue the accessible neighbours
                            q'' = foldr enqueue q' an' 
                            
                            start = case disc of
                                        []        -> defLoc
                                        ((x,_):_) -> x

                            isdest :: Location -> Location -> Direction
                            isdest dest l | dest == l = rwalk dest disc
                                          | otherwise = findPath' m origin dest q'' disc'

                            rwalk :: Location -> [(Location, Location)] -> Direction
                            rwalk _ []                        = undefined -- THIS SHOULD NOT HAPPEN
                            rwalk from route | next == origin = getDirection next curr
                                             | otherwise      = rwalk next route
                                             where
                                                [(curr, next)] = [p | p <- route, fst p == from]
    

    discovered :: [(Location, Location)] -> Location -> Bool
    discovered [] _                     = False
    discovered ((x,_):xs) l | l == x    = True
                            | otherwise = discovered xs l
    -- returns surrounding locations where Ghosts can go                                           
    accessibleNeighbours :: Maze -> Location -> [Location]
    accessibleNeighbours m l = [loc | (loc,field) <- zipped, field /= Wall]
                    where
                        locs  = neighbours l
                        zipped = zip locs (map (getField m) locs)                                                      
                        neighbours :: Location -> [Location]
                        neighbours (x,y) = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]
    
    -- tells if a Ghost can walk a field
    ghostAccessible :: Field -> Bool
    ghostAccessible Wall = False
    ghostAccessible _    = True
    
    -- Returns direction form loc1 to loc2
    getDirection :: Location -> Location -> Direction
    getDirection (x1,y1) (x2,y2) = dir
                    where
                        dists       = [(N, y2-y1), (E, x2-x1), (S, y1-y2), (W, x1-x2)]
                        ((dir,_):_) = sortBy (flip compare `on` snd) dists -- list is hardcoded so cant go wrong
     
    
    oppositeDirection :: Direction -> Direction
    oppositeDirection N = S
    oppositeDirection E = W
    oppositeDirection S = N
    oppositeDirection W = E


    randomDirection :: Maze -> Location -> Direction -> Direction
    randomDirection m loc dir = undefined
    
    updateGhostTimers :: Float -> [Ghost] -> [Ghost]
    updateGhostTimers s = map (updateGhostTimer s)

    updateGhostTimer :: Float -> Ghost -> Ghost
    updateGhostTimer secs (Blinky l d g t) = Blinky l d g (t + secs)
    updateGhostTimer secs (Pinky  l d g t) = Pinky  l d g (t + secs)
    updateGhostTimer secs (Inky   l d g t) = Inky   l d g (t + secs)
    updateGhostTimer secs (Clyde  l d g t) = Clyde  l d g (t + secs)

    im :: Maze
    im = [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,
                    Wall, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Wall,
                    Wall, Item, Wall, Wall, Wall, Item, Wall, Wall, Wall, Item, Wall, Wall, Wall, Item, Wall,
                    Wall, Item, Wall, Item, Item, Item, Wall, Item, Wall, Item, Item, Item, Wall, Item, Wall,
                    Wall, Item, Wall, Item, Wall, Item, Item, Item, Item, Item, Wall, Item, Wall, Item, Wall,
                    Wall, Item, Item, Item, Item, Wall, Wall, Wall, Wall, Wall, Item, Item, Item, Item, Wall,
                    Wall, Item, Wall, Wall, Item, Wall, Spawn, Spawn, Spawn, Wall, Item, Wall, Wall, Item, Wall,
                    Wall, Item, Wall, Item, Item, Wall, Spawn, Spawn, Spawn, Wall, Item, Item, Wall, Item, Wall,
                    Wall, Item, Wall, Wall, Item, Wall, Spawn, Spawn, Spawn, Wall, Item, Wall, Wall, Item, Wall,
                    Wall, Item, Item, Item, Item, Wall, Wall, SpawnDoor, Wall, Wall, Item, Item, Item, Item, Wall,
                    Wall, Item, Wall, Item, Wall, Item, Item, Item, Item, Item, Wall, Item, Wall, Item, Wall,
                    Wall, Item, Wall, Item, Item, Item, Wall, Item, Wall, Item, Item, Item, Wall, Item, Wall,
                    Wall, Item, Wall, Wall, Wall, Item, Wall, Wall, Wall, Item, Wall, Wall, Wall, Item, Wall,
                    Wall, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Item, Wall,
                    Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall
                  ]

    igstate :: Gamestate
    igstate = Gamestate im p initialEnemies d 0 0 0 GameOn
                    where 
                        p@(Pacman _ d _) = initialPacman