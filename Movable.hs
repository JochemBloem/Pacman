module Movable where
    {-
        IMPORTS
     -}
    import Types
    import Queue

    import Data.List (sortBy)
    import Data.Function (on)
    import System.Random
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
        PACMAN 
     -}
    initialPacman :: Pacman
    initialPacman = basePacman 3
    
    initialPacmanLocation :: Location
    initialPacmanLocation = (7,3)

    initialPacmanDirection :: Direction
    initialPacmanDirection = S 

    basePacman :: Int -> Pacman
    basePacman = resetPacman 1 

    resetPacman :: Int -> Int -> Pacman
    resetPacman level l = Pacman initialPacmanLocation initialPacmanDirection pacmanLives
            where 
                pacmanLives  | even level = l + 1
                             | otherwise  = l

    pacmanAccessible :: Field -> Bool
    pacmanAccessible Wall      = False
    pacmanAccessible Spawn     = False
    pacmanAccessible SpawnDoor = False
    pacmanAccessible _         = True
    {- 
        GHOSTS 
     -}

    initialEnemies :: [Ghost]
    initialEnemies = [Blinky (7,10) W Chase 0, Pinky (7,7) S Chase 0, Inky (6,7) N Chase 0, Clyde (8,7) N Chase 0]

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


    -- @TODO: put these things in another file

    -- Implements the BFS Pathfinding algorithm in Haskell
    findPath :: Maze -> Location -> Location -> Direction
    findPath m l1 l2 = findPath' m l1 l2 (enqueue l1 EmptyQ) []

    findPath' :: Maze -> Location -> Location -> Queue Location -> [(Location, Location)] -> Direction
    findPath' m origin dest q disc | continue  = isdest dest l    -- check if we found it, if not: recursion
                                   | otherwise = rwalk start disc -- q is empty, but the destination has not been found. We now calculate the best fitting path
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
                            isdest localdest l | localdest == l = rwalk localdest disc
                                               | otherwise = findPath' m origin localdest q'' disc'

                            rwalk :: Location -> [(Location, Location)] -> Direction
                            rwalk _ []                        = undefined -- THIS SHOULD NOT HAPPEN
                            rwalk curr route | next == origin = getDirection next curr
                                             | otherwise      = rwalk next route
                                             where
                                                [(_, next)] = [p | p <- route, fst p == curr]



    rwalk' :: Location -> Location -> [(Location, Location)] -> Direction
    rwalk' _ _ []                                       = undefined -- THIS SHOULD NOT HAPPEN
    rwalk' from destination route | next == destination = getDirection next curr
                                  | otherwise           = rwalk' next destination route
                        where
                                  [(curr, next)] = [p | p <- route, fst p == from]
                        
    p :: Location
    p = (7,3)

    b1 :: Location
    b1 = (7,10)

    b2 :: Location
    b2 = (6,10)

    b3 :: Location
    b3 = (5,10)

    l1 :: [(Location,Location)]
    l1 = [((7.0,3.0),(7.0,4.0)),((7.0,1.0),(6.0,1.0)),((10.0,1.0),(9.0,1.0)),((8.0,1.0),(9.0,1.0)),((7.0,4.0),(6.0,4.0)),((6.0,1.0),(5.0,1.0)),((4.0,1.0),(3.0,1.0)),((11.0,1.0),(12.0,1.0)),((8.0,4.0),(9.0,4.0)),((9.0,1.0),(9.0,2.0)),((6.0,4.0),(5.0,4.0)),((5.0,1.0),(5.0,2.0)),((3.0,1.0),(2.0,1.0)),((12.0,1.0),(13.0,1.0)),((9.0,4.0),(9.0,3.0)),((9.0,2.0),(9.0,3.0)),((5.0,4.0),(5.0,3.0)),((5.0,2.0),(5.0,3.0)),((2.0,1.0),(1.0,1.0)),((13.0,1.0),(13.0,2.0)),((9.0,3.0),(10.0,3.0)),((5.0,3.0),(4.0,3.0)),((1.0,1.0),(1.0,2.0)),((13.0,2.0),(13.0,3.0)),((10.0,3.0),(11.0,3.0)),((4.0,3.0),(3.0,3.0)),((1.0,2.0),(1.0,3.0)),((13.0,3.0),(13.0,4.0)),((11.0,3.0),(11.0,4.0)),((3.0,3.0),(3.0,4.0)),((1.0,3.0),(1.0,4.0)),((13.0,4.0),(13.0,5.0)),((12.0,5.0),(11.0,5.0)),((11.0,4.0),(11.0,5.0)),((3.0,4.0),(3.0,5.0)),((2.0,5.0),(1.0,5.0)),((1.0,4.0),(1.0,5.0)),((13.0,5.0),(13.0,6.0)),((11.0,5.0),(10.0,5.0)),((3.0,5.0),(4.0,5.0)),((1.0,5.0),(1.0,6.0)),((13.0,6.0),(13.0,7.0)),((10.0,5.0),(10.0,6.0)),((4.0,5.0),(4.0,6.0)),((1.0,6.0),(1.0,7.0)),((13.0,11.0),(13.0,10.0)),((13.0,7.0),(13.0,8.0)),((11.0,7.0),(10.0,7.0)),((10.0,6.0),(10.0,7.0)),((4.0,6.0),(4.0,7.0)),((3.0,7.0),(4.0,7.0)),((1.0,11.0),(1.0,10.0)),((1.0,7.0),(1.0,8.0)),((13.0,12.0),(13.0,13.0)),((13.0,10.0),(13.0,9.0)),((13.0,8.0),(13.0,9.0)),((10.0,7.0),(10.0,8.0)),((1.0,12.0),(1.0,13.0)),((4.0,7.0),(4.0,8.0)),((1.0,10.0),(1.0,9.0)),((1.0,8.0),(1.0,9.0)),((13.0,13.0),(12.0,13.0)),((13.0,9.0),(12.0,9.0)),((10.0,8.0),(10.0,9.0)),((1.0,13.0),(2.0,13.0)),((4.0,8.0),(4.0,9.0)),((1.0,9.0),(2.0,9.0)),((12.0,13.0),(11.0,13.0)),((12.0,9.0),(11.0,9.0)),((10.0,9.0),(11.0,9.0)),((2.0,13.0),(3.0,13.0)),((4.0,9.0),(3.0,9.0)),((2.0,9.0),(3.0,9.0)),((11.0,13.0),(10.0,13.0)),((11.0,9.0),(11.0,10.0)),((7.0,13.0),(6.0,13.0)),((3.0,13.0),(4.0,13.0)),((3.0,9.0),(3.0,10.0)),((10.0,13.0),(9.0,13.0)),((8.0,13.0),(9.0,13.0)),((11.0,10.0),(11.0,11.0)),((6.0,13.0),(5.0,13.0)),((4.0,13.0),(5.0,13.0)),((3.0,10.0),(3.0,11.0)),((9.0,13.0),(9.0,12.0)),((11.0,11.0),(10.0,11.0)),((8.0,6.0),(7.0,6.0)),((6.0,6.0),(6.0,7.0)),((5.0,13.0),(5.0,12.0)),((3.0,11.0),(4.0,11.0)),((9.0,12.0),(9.0,11.0)),((10.0,11.0),(9.0,11.0)),((8.0,7.0),(7.0,7.0)),((7.0,6.0),(7.0,7.0)),((6.0,7.0),(6.0,8.0)),((5.0,12.0),(5.0,11.0)),((4.0,11.0),(5.0,11.0)),((9.0,11.0),(9.0,10.0)),((8.0,8.0),(7.0,8.0)),((7.0,7.0),(7.0,8.0)),((6.0,8.0),(7.0,8.0)),((5.0,11.0),(5.0,10.0)),((9.0,10.0),(8.0,10.0)),((7.0,8.0),(7.0,9.0)),((7.0,10.0),(6.0,10.0)),((5.0,10.0),(6.0,10.0)),((7.0,11.0),(7.0,10.0)),((8.0,10.0),(7.0,10.0)),((7.0,9.0),(7.0,10.0)),((6.0,10.0),(7.0,10.0))]

    l2 :: [(Location,Location)]
    l2 = [((10.0,1.0),(9.0,1.0)),((8.0,1.0),(7.0,1.0)),((11.0,1.0),(12.0,1.0)),((9.0,1.0),(9.0,2.0)),((8.0,4.0),(7.0,4.0)),((7.0,3.0),(7.0,4.0)),((7.0,1.0),(6.0,1.0)),((12.0,1.0),(13.0,1.0)),((9.0,4.0),(9.0,3.0)),((9.0,2.0),(9.0,3.0)),((7.0,4.0),(6.0,4.0)),((6.0,1.0),(5.0,1.0)),((4.0,1.0),(3.0,1.0)),((13.0,1.0),(13.0,2.0)),((9.0,3.0),(10.0,3.0)),((6.0,4.0),(5.0,4.0)),((5.0,1.0),(5.0,2.0)),((3.0,1.0),(2.0,1.0)),((13.0,2.0),(13.0,3.0)),((10.0,3.0),(11.0,3.0)),((5.0,4.0),(5.0,3.0)),((5.0,2.0),(5.0,3.0)),((2.0,1.0),(1.0,1.0)),((13.0,3.0),(13.0,4.0)),((11.0,3.0),(11.0,4.0)),((5.0,3.0),(4.0,3.0)),((1.0,1.0),(1.0,2.0)),((13.0,4.0),(13.0,5.0)),((12.0,5.0),(11.0,5.0)),((11.0,4.0),(11.0,5.0)),((4.0,3.0),(3.0,3.0)),((1.0,2.0),(1.0,3.0)),((13.0,5.0),(13.0,6.0)),((11.0,5.0),(10.0,5.0)),((3.0,3.0),(3.0,4.0)),((1.0,3.0),(1.0,4.0)),((13.0,6.0),(13.0,7.0)),((10.0,5.0),(10.0,6.0)),((3.0,4.0),(3.0,5.0)),((2.0,5.0),(1.0,5.0)),((1.0,4.0),(1.0,5.0)),((13.0,11.0),(13.0,10.0)),((13.0,7.0),(13.0,8.0)),((11.0,7.0),(10.0,7.0)),((10.0,6.0),(10.0,7.0)),((3.0,5.0),(4.0,5.0)),((1.0,5.0),(1.0,6.0)),((13.0,12.0),(13.0,13.0)),((13.0,10.0),(13.0,9.0)),((13.0,8.0),(13.0,9.0)),((10.0,7.0),(10.0,8.0)),((4.0,5.0),(4.0,6.0)),((1.0,6.0),(1.0,7.0)),((13.0,13.0),(12.0,13.0)),((13.0,9.0),(12.0,9.0)),((10.0,8.0),(10.0,9.0)),((4.0,6.0),(4.0,7.0)),((3.0,7.0),(4.0,7.0)),((1.0,11.0),(1.0,10.0)),((1.0,7.0),(1.0,8.0)),((12.0,13.0),(11.0,13.0)),((12.0,9.0),(11.0,9.0)),((10.0,9.0),(11.0,9.0)),((1.0,12.0),(1.0,13.0)),((4.0,7.0),(4.0,8.0)),((1.0,10.0),(1.0,9.0)),((1.0,8.0),(1.0,9.0)),((11.0,13.0),(10.0,13.0)),((11.0,9.0),(11.0,10.0)),((1.0,13.0),(2.0,13.0)),((4.0,8.0),(4.0,9.0)),((1.0,9.0),(2.0,9.0)),((10.0,13.0),(9.0,13.0)),((11.0,10.0),(11.0,11.0)),((8.0,13.0),(7.0,13.0)),((2.0,13.0),(3.0,13.0)),((4.0,9.0),(3.0,9.0)),((2.0,9.0),(3.0,9.0)),((9.0,13.0),(9.0,12.0)),((11.0,11.0),(10.0,11.0)),((8.0,6.0),(7.0,6.0)),((6.0,6.0),(6.0,7.0)),((7.0,13.0),(6.0,13.0)),((3.0,13.0),(4.0,13.0)),((3.0,9.0),(3.0,10.0)),((9.0,12.0),(9.0,11.0)),((10.0,11.0),(9.0,11.0)),((8.0,7.0),(7.0,7.0)),((7.0,6.0),(7.0,7.0)),((6.0,7.0),(6.0,8.0)),((6.0,13.0),(5.0,13.0)),((4.0,13.0),(5.0,13.0)),((3.0,10.0),(3.0,11.0)),((9.0,11.0),(9.0,10.0)),((8.0,8.0),(7.0,8.0)),((7.0,7.0),(7.0,8.0)),((6.0,8.0),(7.0,8.0)),((5.0,13.0),(5.0,12.0)),((3.0,11.0),(4.0,11.0)),((9.0,10.0),(8.0,10.0)),((7.0,8.0),(7.0,9.0)),((5.0,12.0),(5.0,11.0)),((4.0,11.0),(5.0,11.0)),((7.0,11.0),(7.0,10.0)),((8.0,10.0),(7.0,10.0)),((7.0,9.0),(7.0,10.0)),((5.0,11.0),(5.0,10.0)),((6.0,10.0),(5.0,10.0)),((7.0,10.0),(6.0,10.0)),((5.0,10.0),(6.0,10.0))]

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


    randomDirection :: Maze -> Location -> Direction
    randomDirection m loc = getDirection loc newLoc
                        where
                            nbs    = accessibleNeighbours m loc
                            newLoc = nbs!!getRandomNumber 0 (length nbs)


    getRandomNumber :: Int -> Int -> Int
    getRandomNumber upperLimit lowerLimit = lowerLimit -- @TODOJOCHEM: actueel willekeurig getal                       

    
    updateGhostTimers :: Float -> [Ghost] -> [Ghost]
    updateGhostTimers s = map (updateGhostTimer s)

    updateGhostTimer :: Float -> Ghost -> Ghost
    updateGhostTimer secs (Blinky l d g t) = Blinky l d g (t + secs)
    updateGhostTimer secs (Pinky  l d g t) = Pinky  l d g (t + secs)
    updateGhostTimer secs (Inky   l d g t) = Inky   l d g (t + secs)
    updateGhostTimer secs (Clyde  l d g t) = Clyde  l d g (t + secs)

    im :: Maze
    im = [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,
        Wall, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Wall,
        Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall,
        Wall, Dot, Wall, Energizer, Dot, Dot, Wall, Dot, Wall, Dot, Dot, Energizer, Wall, Dot, Wall,
        Wall, Dot, Wall, Dot, Wall, Dot, Dot, Dot, Dot, Dot, Wall, Dot, Wall, Dot, Wall,
        Wall, Dot, Dot, Dot, Dot, Wall, Wall, Wall, Wall, Wall, Dot, Dot, Dot, Dot, Wall,
        Wall, Dot, Wall, Wall, Dot, Wall, Spawn, Spawn, Spawn, Wall, Dot, Wall, Wall, Dot, Wall,
        Wall, Dot, Wall, Dot, Dot, Wall, Spawn, Spawn, Spawn, Wall, Dot, Dot, Wall, Dot, Wall,
        Wall, Dot, Wall, Wall, Dot, Wall, Spawn, Spawn, Spawn, Wall, Dot, Wall, Wall, Dot, Wall,
        Wall, Dot, Dot, Dot, Dot, Wall, Wall, SpawnDoor, Wall, Wall, Dot, Dot, Dot, Dot, Wall,
        Wall, Dot, Wall, Dot, Wall, Dot, Dot, Dot, Dot, Dot, Wall, Dot, Wall, Dot, Wall,
        Wall, Dot, Wall, Energizer, Dot, Dot, Wall, Empty, Wall, Dot, Dot, Energizer, Wall, Dot, Wall,
        Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall,
        Wall, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Wall,
        Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall
        ]

    igstate :: Gamestate
    igstate = Gamestate im p initialEnemies d 0 0 0 GameOn
                    where 
                        p@(Pacman _ d _) = initialPacman