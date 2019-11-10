module Main where

    import Controller
    import View
    import Initials
    import Types
    
    import Graphics.Gloss.Interface.IO.Game
    
    -- launch app
    main :: IO ()
    main = playIO (InWindow "Pacman" screenSizeI (0, 0)) -- Or FullScreen
                  black            -- Background color
                  30               -- Frames per second
                  initialGameState -- Initial state
                  view             -- View  function
                  input            -- Event function
                  step             -- Step  function