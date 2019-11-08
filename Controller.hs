-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Types
    import Movable
    import HelperFunctions
    import Initials
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    
    -- | Handle one iteration of the game
    step :: Float -> Gamestate -> IO Gamestate
    step _ gstate@(Gamestate _ _ _ _ _ _ _ Paused)                        = return gstate
    step _ gstate@(Gamestate _ _ _ _ _ _ _ GameOver)                      = return gstate -- @TODO: filesystem
    step secs gstate | mazeEmpty                                          = return $ (resetGameState (score newGstate + 500) (lvl + 1))  { player = resetPacman lvl (lives $ player gstate) }
                     | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = return $ finalGstate { elapsedTime = 0
                                                                                                 , enemies     = newGhosts
                                                                                                 }
                     | otherwise                                          = return $ newGstate   { elapsedTime = elapsedTime gstate + secs 
                                                                                                 }
        where  
          -- often used variables
          pacman    = player  gstate
          pacLoc    = location pacman
          m         = maze    gstate
          ghosts    = enemies gstate
          lvl       = level   gstate
          stat      = status  gstate
          notScaredGhosts = filter isNotScared ghosts
          eatenGhosts     = map (isEaten pacLoc) ghosts
          newScore        = score gstate + 100000 * amountInList True eatenGhosts -- I think this should work @TODO @HEELP

          -- updated pacman
          newPacman = move newPacman' m
              where
                  newPacman' | isOnTile pacLoc = changeDirection pacman (newDir gstate) (maze gstate)
                             | otherwise       = pacman

          pacmanDies :: Bool         
          pacmanDies = roundedLocation (location newPacman) `elem` map (roundLoc . getGhostLocation) notScaredGhosts

          -- updated ghosts
          newGhosts   | pacmanDies = initialEnemies
                      | otherwise  = updateGhostTimers secs (map (`move` m) (aiSteps gstate behaveGhosts) )
                            where
                              behaveGhosts = map updateGhostBehaviour ghosts
          
          -- updated score and maze
          newGstate   | isOnTile pacLoc = updateGstate pacLoc
                      | otherwise       = gstate { score = newScore }
          
          finalGstate | pacmanDies = newGstate { player  = basePacman newLives
                                               , status  = newStatus
                                               }
                      | otherwise  = newGstate { player  = newPacman
                                               }
                      where
                        newLives   = lives (player gstate) - 1
                        newStatus  | newLives <= 0 = GameOver
                                   | otherwise     = GameOn
                        
          -- Local functions
          updateGstate :: Location -> Gamestate
          updateGstate loc = doFieldAction f                                    
                          where
                            i = locationToIndex loc
                            f = getField m loc

                            doFieldAction :: Field ->  Gamestate
                            doFieldAction Dot       = gstate { score = newScore +  10, maze = clearLoc }
                            doFieldAction Energizer = gstate { score = newScore +  50, maze = clearLoc, enemies = scaredGhosts }
                            doFieldAction Fruit     = gstate { score = newScore + 100, maze = clearLoc }
                            doFieldAction _         = gstate { score = newScore }

                            clearLoc :: Maze
                            clearLoc = setField m loc Empty

                            scaredGhosts :: [Ghost]
                            scaredGhosts = map (setGhostBehaviour Frightened) (enemies gstate)
                            
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
          update gstate timer
          update ghosttimers (if GhostBehaviour != frightened)
     -}


    -- | Handle user input
    input :: Event -> Gamestate -> IO Gamestate
    input e gstate = return (inputKey e gstate)

    inputKey :: Event -> Gamestate -> Gamestate
    inputKey (EventKey (Char 'w') _ _ _) gstate = gstate { newDir = N }
    inputKey (EventKey (Char 'a') _ _ _) gstate = gstate { newDir = W }
    inputKey (EventKey (Char 's') _ _ _) gstate = gstate { newDir = S }
    inputKey (EventKey (Char 'd') _ _ _) gstate = gstate { newDir = E }
    inputKey (EventKey (Char 'p') _ _ _) gstate = gstate { status = Paused }
    inputKey (EventKey (Char 'u') _ _ _) gstate = gstate { status = GameOn }
    inputKey (EventKey (Char 'g') _ _ _) gstate = gstate { status = GameOver } -- @TODO: remove, just for debug sake
    inputKey (EventKey (Char 'r') _ _ _) gstate = initialGameState
    inputKey _                           gstate = gstate -- Otherwise keep the same