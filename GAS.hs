{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Square Color Heading | Circle Color| Arrow Heading
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show (Square col hd) | col == Red = "R" ++ show hd
                         | col == Blue = "B" ++ show hd
                         | col == Gray = "G" ++ show hd
                         | otherwise = ""

    show (Circle col)    | col == Red = "r"
                         | col == Blue = "b"
                         | col == Gray = "g"
                         | otherwise = ""

    show (Arrow hd) = show hd
{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = Level {levelMap :: M.Map Position [Object]}
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}
getLevelBounds :: Level -> [Position]
getLevelBounds lev = foldl (\acc pos -> let minX = if (fst pos) < (fst (head acc)) then fst pos else fst (head acc);
                                            minY = if (snd pos) < (snd (head acc)) then snd pos else snd (head acc);
                                            maxX = if (fst pos) > (fst (last acc)) then fst pos else fst (last acc);
                                            maxY = if (snd pos) > (snd (last acc)) then snd pos else snd (last acc);
                                            in [(minX, minY), (maxX, maxY)] )
                       [(maxBound, maxBound), (minBound, minBound)] 
                       (M.foldrWithKey (\pos _ acc -> pos:acc) [] (levelMap lev))


instance Show Level where
    show lev = let bounds = getLevelBounds lev
                   coordsX = (fst (head bounds), fst (last bounds))
                   coordsY = (snd (head bounds), snd (last bounds))
                   resultString = foldl (\acc pos -> case M.lookup pos (levelMap lev) of
                   	                    Nothing -> if (snd pos) == (snd coordsY) then acc ++ "   \n" else acc ++ "   |"
                   	                    Just shapes -> case shapes of
			                	                    	[] -> acc
			                	                    	(x:xs) -> case xs of
			                	                    		        [] -> case x of
			                	                    		              Square c h -> acc ++ show (Square c h) ++ (if (snd pos) == (snd coordsY) then " \n" else " |")
			                	                    		              t -> acc ++ "  " ++ show t ++ (if (snd pos) == (snd coordsY) then "\n" else "|")
			                	                    		        (y:_) -> acc ++ show x ++ show y ++ (if (snd pos) == (snd coordsY) then "\n" else "|")
                	) 
                      "" 
                      [(x, y) | x <- [(fst coordsX) .. (snd coordsX)], y <- [(fst coordsY) .. (snd coordsY)]]
                in
                	take ((length resultString) - 1) resultString
                

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = Level M.empty

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color hd pos lev =  
    case M.lookup pos (levelMap lev) of
        Nothing -> Level (M.insert pos [(Square color hd)]  (levelMap lev))
        Just _ -> Level (M.adjust ([(Square color hd)] ++) pos (levelMap lev))


{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color pos lev =  
    case M.lookup pos (levelMap lev) of
        Nothing -> Level (M.insert pos [(Circle color)]  (levelMap lev))
        Just _ -> Level (M.adjust ([(Circle color)] ++) pos (levelMap lev))
{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow hd pos lev =  
    case M.lookup pos (levelMap lev) of
        Nothing -> Level (M.insert pos [(Arrow hd)]  (levelMap lev))
        Just _ -> Level (M.adjust ([(Arrow hd)] ++) pos (levelMap lev))

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final


move pos lev = let newPosition = \ position hd -> case hd of
                                               North -> ((fst position) - 1, (snd position))
                                               South -> ((fst position) + 1, (snd position))
                                               East ->  ((fst position), (snd position) + 1)
                                               West ->  ((fst position), (snd position) - 1)
               in
               case M.lookup pos (levelMap lev) of
               	Nothing -> lev
               	Just shapes -> case shapes of 
               		            [] -> lev
               		            (x:xs) -> case xs of
               		            	       [] -> case x of 
               		            	       	      Square c h -> case M.lookup (newPosition pos h) (levelMap lev) of
               		            	       	      	             Nothing -> addSquare c h (newPosition pos h) (Level (M.delete pos (levelMap lev)))
               		            	       	      	             Just newShapes -> case newShapes of
               		            	       	      	             	                 [] -> lev
               		            	       	      	             	                 (z:zs) -> case zs of
               		            	       	      	             	                 	        [] -> case z of
               		            	       	      	             	                 	        	   Square _ _ -> (move (newPosition pos h) lev)
               		            	       	      	             	                 	        	   Circle _ -> Level (M.adjust ([(Square c h)] ++) (newPosition pos h) (M.delete pos (levelMap lev)))
               		            	       	      	             	                 	        	   Arrow aHead -> Level (M.adjust ([(Square c aHead)] ++) (newPosition pos h) (M.delete pos (levelMap lev)))
               		            	       	      	             	                 	        (r:_) -> lev {- TODO pe noua pozitie se mai afla un cerc sau sageata pe langa patrat -}
               		            	       	      _ -> lev
               		            	       (y:_) -> case x of 
               		            	       	         Square colr hed -> case M.lookup (newPosition pos hed) (levelMap lev) of
               		            	       	         	                 Nothing -> addSquare colr hed (newPosition pos hed) (Level (M.adjust (tail) pos (levelMap lev)))
               		            	       	         	                 Just newShapes -> case newShapes of
               		            	       	         	                 	                 [] -> lev
               		            	       	         	                 	                 (z:zs) -> case zs of
               		            	       	         	                 	                 	        [] -> case z of
               		            	       	         	                 	                 	        	   Square _ _ -> lev
               		            	       	         	                 	                 	        	   Circle _ -> Level (M.adjust ([(Square colr hed)] ++) (newPosition pos hed) (M.adjust (tail) pos (levelMap lev)))
               		            	       	         	                 	                 	        	   Arrow aHead -> Level (M.adjust ([(Square colr aHead)] ++) (newPosition pos hed) (M.adjust (tail) pos (levelMap lev)))
               		            	       	         	                 	                 	        (r:_) -> lev {- TODO pe noua pozitie se mai afla un cerc sau sageata pe langa patrat -}
               		            	       	         _ -> lev     	             	                 	        
               		            	                

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic =
