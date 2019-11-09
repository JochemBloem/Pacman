module Initials where 

    {- 
        IMPORTS 
     -}
    import Types

    {- 
        GAMESTATE 
     -}

    initialGameState :: Gamestate
    initialGameState = resetGameState 0 1

    resetGameState :: Int -> Int -> Gamestate
    resetGameState score level = Gamestate initialMaze p initialEnemies d score level 0 GameOn
            where 
                p@(Pacman _ d _ _) = initialPacman
    {- 
        MAZE 
     -}
    initialMaze :: Maze
    initialMaze = [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,
                    Wall, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Wall,
                    Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall,
                    Wall, Dot, Wall, Energizer, Dot, Dot, Wall, Empty, Wall, Dot, Dot, Energizer, Wall, Dot, Wall,
                    Wall, Dot, Wall, Dot, Wall, Dot, Dot, Dot, Dot, Dot, Wall, Dot, Wall, Dot, Wall,
                    Wall, Dot, Dot, Dot, Dot, Wall, Wall, Wall, Wall, Wall, Dot, Dot, Dot, Dot, Wall,
                    Wall, Dot, Wall, Wall, Dot, Wall, Spawn, Spawn, Spawn, Wall, Dot, Wall, Wall, Dot, Wall,
                    Wall, Dot, Wall, Dot, Dot, Wall, Spawn, Spawn, Spawn, Wall, Dot, Dot, Wall, Dot, Wall,
                    Wall, Dot, Wall, Wall, Dot, Wall, Spawn, Spawn, Spawn, Wall, Dot, Wall, Wall, Dot, Wall,
                    Wall, Dot, Dot, Dot, Dot, Wall, Wall, SpawnDoor, Wall, Wall, Dot, Dot, Dot, Dot, Wall,
                    Wall, Dot, Wall, Dot, Wall, Dot, Dot, Dot, Dot, Dot, Wall, Dot, Wall, Dot, Wall,
                    Wall, Dot, Wall, Energizer, Dot, Dot, Wall, Fruit, Wall, Dot, Dot, Energizer, Wall, Dot, Wall,
                    Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall, Wall, Wall, Dot, Wall,
                    Wall, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Wall,
                    Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall
                  ]
    -- (Almost) empty maze, for testing purposes
    almostEmptyMaze :: Maze
    almostEmptyMaze = [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,
                        Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Dot, Empty, Empty, Empty, Empty, Empty, Wall,
                        Wall, Empty, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Empty, Wall,
                        Wall, Empty, Wall, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Empty, Empty, Wall, Empty, Wall,
                        Wall, Empty, Wall, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Wall,
                        Wall, Empty, Empty, Empty, Empty, Wall, Wall, Wall, Wall, Wall, Empty, Empty, Empty, Empty, Wall,
                        Wall, Empty, Wall, Wall, Empty, Wall, Spawn, Spawn, Spawn, Wall, Empty, Wall, Wall, Empty, Wall,
                        Wall, Empty, Wall, Empty, Empty, Wall, Spawn, Spawn, Spawn, Wall, Empty, Empty, Wall, Empty, Wall,
                        Wall, Empty, Wall, Wall, Empty, Wall, Spawn, Spawn, Spawn, Wall, Empty, Wall, Wall, Empty, Wall,
                        Wall, Empty, Empty, Empty, Empty, Wall, Wall, SpawnDoor, Wall, Wall, Empty, Empty, Empty, Empty, Wall,
                        Wall, Empty, Wall, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall, Empty, Wall,
                        Wall, Empty, Wall, Empty, Empty, Empty, Wall, Fruit, Wall, Empty, Empty, Empty, Wall, Empty, Wall,
                        Wall, Empty, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Empty, Wall, Wall, Wall, Empty, Wall,
                        Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall,
                        Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall
                    ]   
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
    resetPacman level l = Pacman initialPacmanLocation initialPacmanDirection pacmanLives 45
            where 
                pacmanLives | even level = l + 1
                            | otherwise  = l

    {- 
        GHOSTS 
     -}

    initialEnemies :: [Ghost]
    --initialEnemies = [Blinky (7,10) W Chase 0, Pinky (7,7) S Chase 0, Inky (6,7) N Chase 0, Clyde (8,7) N Chase 0]
    initialEnemies = [baseBlinky 0, basePinky 0, baseInky 0, baseClyde 0]

    baseBlinky :: Float -> Ghost
    baseBlinky t = Blinky (7,10) W Chase t 0 
    basePinky  :: Float -> Ghost
    basePinky  t = Pinky  (7,7)  S Chase t 2500
    baseInky   :: Float -> Ghost
    baseInky   t = Inky   (6,7)  N Chase t 500
    baseClyde  :: Float -> Ghost
    baseClyde  t = Clyde  (8,7)  N Chase t 7500

    -- Scatter locations
    scatterLocation :: Ghost -> Location
    scatterLocation Blinky {} = boardSizeF -- Upper right corner
    scatterLocation Pinky  {} = ( 0, by)   -- upper left  corner
                           where
                            (_,by) = boardSizeF
    scatterLocation Inky   {} = (bx,  0)   -- Lower right corner 
                         where
                            (bx,_) = boardSizeF
    scatterLocation Clyde  {} = ( 0,  0)   -- Lower left  corner
