module Main where

    import Controller
    import View
    import General
    import Types
    
    import Graphics.Gloss.Interface.IO.Game
    
    
    main :: IO ()
    main = playIO (InWindow "Pacman" screenSizeI (0, 0)) -- Or FullScreen
                  black            -- Background color
                  30               -- Frames per second
                  initialGameState -- Initial state
                  view             -- View  function
                  input            -- Event function
                  step             -- Step  function

