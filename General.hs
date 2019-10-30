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
    initialGameState = Gamestate initialMaze p initialEnemies d 0 0 0 GameOn
                    where 
                        p@(Pacman _ d _) = initialPacman

    {- 
        MAZE 
     -}
    initialMaze :: Maze
    initialMaze = [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,
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


    {-
        GENERAL FUNCITONS
     -}
    
    -- toPoint :: Location -> Point
    -- toPoint (x,y) = Point x y