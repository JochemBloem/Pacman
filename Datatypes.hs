module Datatypes where  
    initialGameState :: Gamestate
    initialGameState = Gamestate initialMaze initialPacman initialEnemies 0 0 GameOn
    
    initialMaze :: Maze
    initialMaze = [[Wall, Wall, Wall],[Wall, Item , Wall],[Wall, Wall, Wall]]

    initialPacman :: Pacman
    initialPacman = Pacman initialPacmanLocation N 3
    
    initialPacmanLocation :: Location
    initialPacmanLocation = (0,0)


    initialEnemies :: [Ghost]
    initialEnemies = [Blinky (1,1) N Chase 0]
    
    data Gamestate = Gamestate  {   maze           :: Maze
                                ,   player         :: Pacman
                                ,   enemies        :: [Ghost]
                                ,   score          :: Int 
                                ,   level          :: Int
                                ,   status         :: Status
                                }
    data Status = Paused | GameOn | GameOver


    
    type Location  = (Int, Int)
    data Direction = N | E | S | W
    
    data Pacman = Pacman { location  :: Location
                         , direction :: Direction
                         , lives     :: Int
                         }

    type Maze = [[Field]]
    data Field = Wall   | Spawn      | Item
    data Item  = Empty  | Dot        | Energizer | Fruit
    data Fruit = Cherry | Strawberry | Orange    | Apple | Melon | Galaxian | Bell | Key

    data Ghost = Blinky Location Direction GhostBehaviour Int
               | Pinky  Location Direction GhostBehaviour Int
               | Inky   Location Direction GhostBehaviour Int
               | Clyde  Location Direction GhostBehaviour Int

    data GhostBehaviour = Chase  | Frightened | Scatter
