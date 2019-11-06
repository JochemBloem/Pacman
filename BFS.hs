module BFS where
    {-
        IMPORTS
     -}
    import Types
    import Queue
    import HelperFunctions

    import Data.Sort


    -- Implements the BFS Pathfinding algorithm in Haskell
    findPath :: Maze -> Location -> Location -> Direction
    findPath m l1 l2 = findPath' m l1 l2 (enqueue l1 EmptyQ) []

    findPath' :: Maze -> Location -> Location -> Queue Location -> [(Location, Location)] -> Direction
    findPath' m origin dest q disc | continue  = isdest dest l      -- check if we found it, if not: recursion
                                   | otherwise = rwalk closest disc -- q is empty, but the destination has not been found. We now calculate the best fitting path
                        where 
                            (ml, q') = dequeue q
                            (continue, l) = case ml of 
                                        Just l  -> (True,  l     )
                                        Nothing -> (False, defLoc)
                                        
                            an    = accessibleNeighbours m l
                            an'   = filter (not . discovered disc) an
                            
                            -- add the to discovered list
                            disc' = [(n,l) | n <- an'] ++ disc
                            -- enqueue the accessible neighbours
                            q'' = foldr enqueue q' an' 
                            
                            ((_,closest):_) = sort [((`distance` dest) loc, loc) | (loc,_) <- disc]

                            isdest :: Location -> Location -> Direction
                            isdest localdest l | localdest == l = rwalk localdest disc
                                               | otherwise      = findPath' m origin localdest q'' disc'

                            rwalk :: Location -> [(Location, Location)] -> Direction
                            rwalk _ []                        = undefined -- THIS SHOULD NOT HAPPEN
                            rwalk curr route | next == origin = getDirection next curr
                                             | otherwise      = rwalk next route
                                             where
                                                [(_, next)] = [p | p <- route, fst p == curr]

                                                
    discovered :: [(Location, Location)] -> Location -> Bool
    discovered [] _                     = False
    discovered ((x,_):xs) l | l == x    = True
                            | otherwise = discovered xs l
    -- returns surrounding locations where Ghosts can go                                           
    accessibleNeighbours :: Maze -> Location -> [Location]
    accessibleNeighbours m l = [loc | (loc,field) <- zipped, field /= Wall]
                    where
                        locs  = neighbours l
                        zipped = zip locs (map (getField m) locs)                                                      
                        neighbours :: Location -> [Location]
                        neighbours (x,y) = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]