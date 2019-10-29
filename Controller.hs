-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Model
    import Types
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    
    -- | Handle one iteration of the game
    step :: Float -> Gamestate -> IO Gamestate
    step _ = return
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
    input _ = return
    {-
    input e gstate = return (inputKey e gstate)

    inputKey :: Event -> GameState -> GameState
    inputKey (EventKey (Char c) _ _ _) gstate
      = -- If the user presses a character key, show that one
        gstate { infoToShow = ShowAChar c }
    inputKey _ gstate = gstate -- Otherwise keep the same
    
  -}