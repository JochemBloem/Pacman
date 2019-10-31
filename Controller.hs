-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Types
    import Movable
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    -- import System.Random
    
    -- | Handle one iteration of the game
    step :: Float -> Gamestate -> IO Gamestate
    step secs gstate | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = return $ gstate { player      = newPacman                , elapsedTime = 0, enemies = newGhosts }
                     | otherwise                                          = return $ gstate { elapsedTime = elapsedTime gstate + secs }
        where  
          pacman                                        = player gstate
          newPacman                                     = move newPacman' (maze gstate)
          newPacman' | pacmanIsOnTile (location pacman) = changeDirection pacman (newDir gstate) (maze gstate)
                     | otherwise                        = pacman
          newGhosts                                     = map (flip move $ maze gstate) (aiSteps gstate $ enemies gstate) 
          
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

    