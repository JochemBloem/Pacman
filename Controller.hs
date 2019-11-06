-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Types
    import Movable
    import General
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    -- import System.Random
    
    -- | Handle one iteration of the game
    step :: Float -> Gamestate -> IO Gamestate
    
    step secs gstate | mazeEmpty                                          = return $ (resetGameState (score newGstate + 500) (lvl + 1))  { player = resetPacman lvl (lives $ player gstate) }
                     | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = return $ finalGstate { elapsedTime = 0
                                                                                                 , enemies     = newGhosts
                                                                                                 }
                     | otherwise                                          = return $ newGstate   { elapsedTime = elapsedTime gstate + secs 
                                                                                                 }
        where  
          -- often used variables
          pacman    = player gstate
          pacLoc    = location pacman
          m         = maze gstate
          ghostLocs = map getGhostLocation (enemies gstate)
          lvl       = level gstate

          -- updated pacman
          newPacman = move newPacman' m
              where
                  newPacman' | isOnTile pacLoc = changeDirection pacman (newDir gstate) (maze gstate)
                             | otherwise       = pacman
          pacmanDies = roundedLocation (location newPacman) `elem` map roundedLocation ghostLocs
          -- updated ghosts
          newGhosts   | pacmanDies = initialEnemies
                      | otherwise  = map (`move` m) (aiSteps gstate $ enemies gstate) 
          -- updated score and maze
          newGstate   | isOnTile pacLoc = updateGstate pacLoc
                      | otherwise       = gstate
          
          finalGstate | pacmanDies = newGstate { player  = basePacman newLives
                                               , status  = newStatus
                                               }
                      | otherwise  = newGstate { player  = newPacman
                                               }
                      where
                        newLives   = lives (player gstate) - 1
                        newStatus  | newLives == 0 = GameOver
                                   | otherwise     = GameOn
                        
          -- Local functions
          updateGstate :: Location -> Gamestate
          updateGstate loc = doFieldAction f                                    
                          where
                            i = locationToIndex loc
                            f = getField m loc

                            doFieldAction :: Field ->  Gamestate
                            doFieldAction Dot       = gstate { score = score gstate +  10, maze = clearLoc }
                            doFieldAction Energizer = gstate { score = score gstate +  50, maze = clearLoc }
                            doFieldAction Fruit     = gstate { score = score gstate + 100, maze = clearLoc }
                            doFieldAction _         = gstate
                            
                            clearLoc :: Maze
                            clearLoc = setMazeField m loc Empty
                            
          mazeEmpty :: Bool --     Allowed : Wall Spawn SpawnDoor Empty Fruit
                            -- Not Allowed : Energizer Dot
          mazeEmpty = all allowed m
                    where
                      allowed :: Field -> Bool
                      allowed Energizer = False
                      allowed Dot       = False
                      allowed _         = True
          {-
          TODO step
          Update locations (pacman and ghost)
          update gstate timer
          update ghosttimers (if GhostBehaviour != frightened)
          check pacmans field AND newDir
          check pacmans ghosts
     -}


{- 
    step secs gstate
      | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
      = -- We show a new random number
        do randomNumber <- randomIO
           let newNumber = abs randomNumber `mod` 10
           return $ GameState (ShowANumber newNumber) 0
      | otherwise
      = -- Just update the elapsed time
        return $ gstate { elapsedTime = elapsedTime gstate + secs }
    -}
    -- | Handle user input
    input :: Event -> Gamestate -> IO Gamestate
    input e gstate = return (inputKey e gstate)

    inputKey :: Event -> Gamestate -> Gamestate
    inputKey (EventKey (Char 'w') _ _ _) gstate = gstate { newDir = N }
    inputKey (EventKey (Char 'a') _ _ _) gstate = gstate { newDir = W }
    inputKey (EventKey (Char 's') _ _ _) gstate = gstate { newDir = S }
    inputKey (EventKey (Char 'd') _ _ _) gstate = gstate { newDir = E }
    inputKey _ gstate = gstate -- Otherwise keep the same
    {-
    input e gstate = return (inputKey e gstate)

    inputKey :: Event -> GameState -> GameState
    inputKey (EventKey (Char c) _ _ _) gstate
      = -- If the user presses a character key, show that one
        gstate { infoToShow = ShowAChar c }
    inputKey _ gstate = gstate -- Otherwise keep the same
    
  -}

    