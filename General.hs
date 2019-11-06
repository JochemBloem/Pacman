module General where 

    {- 
        IMPORTS 
     -}
    import Types
    import Movable
    import System.IO
    import Control.Monad
    {- 
        GAMESTATE 
     -}

    initialGameState :: Gamestate
    initialGameState = resetGameState 0 1

    resetGameState :: Int -> Int -> Gamestate
    resetGameState score level = Gamestate almostEmptyMaze p initialEnemies d score level 0 GameOn
            where 
                p@(Pacman _ d _) = initialPacman
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
        GENERAL FUNCITONS
     -}
    
    -- toPoint :: Location -> Point
    -- toPoint (x,y) = Point x y