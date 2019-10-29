-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Model
    import Types
    
    view :: Gamestate -> IO Picture
    view  = return . viewPure


    viewPure :: Gamestate -> Picture
    viewPure gstate = case status gstate of
                      Paused   -> color blue (Translate ((-1)*sx/2) 0 $ Text "PAUSED")
                                where 
                                  (sx, sy) = screenSizeF
                      GameOn   -> Pictures $ viewPure' $ maze gstate
                      GameOver -> Scale 0.5 0.5 $ color red (Translate ((-1)*sx/2) 0 $ Text "GAME OVER")
                                where 
                                  (sx, sy) = screenSizeF
                      

    viewPure' :: Maze -> [Picture]
    viewPure' m = zipWith (curry fieldPicture) m [0..]

    fieldPicture :: (Field, Int) -> Picture
    fieldPicture (Wall,      i) = color blue (Translate dx dy $ rectangleWire fieldSize fieldSize )
                  where 
                    (dx, dy)    = translateTo i
    fieldPicture (Spawn,     i) = Blank
    fieldPicture (SpawnDoor, i) = color red (Line [(dx-w, dy), (dx+w,dy)])
                  where 
                    (dx, dy)    = translateTo i
                    w           = fieldSize / 2
    fieldPicture (Item,      i) = color yellow (Translate dx dy $ ThickCircle (scaledFieldSize 0.15 / 2) 1)
                  where 
                    (dx, dy)    = translateTo i

    translateTo :: Int -> (Float, Float)
    translateTo i = (dx, dy)
                    where
                      (width,height) = sizeF
                      w2             = max width height / 2
                      d              = fieldSize
                      (x,y)          = indexToLocation i
                      dx             = x * d - w2 * d
                      dy             = y * d - w2 * d

    fieldSize :: Float 
    fieldSize = scaledFieldSize 1

    scaledFieldSize :: Float -> Float
    scaledFieldSize s = 30 * s
    
    {-
    view :: GameState -> IO Picture
    view = return . viewPure
    
    viewPure :: GameState -> Picture
    viewPure gstate = case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowAChar   c -> color green (text [c])
     -}