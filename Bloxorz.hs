{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
-- import Data.Matrix
module Bloxorz where

import ProblemState
import qualified Data.Array as A

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)
{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

data Cell = HardTile | SoftTile | Block | Switch | EmptySpace | WinningTile deriving (Eq, Ord)


instance Show Cell where
    show HardTile =  [hardTile]
    show SoftTile = [softTile]
    show Block = [block]
    show Switch = [switch]
    show EmptySpace = [emptySpace]
    show WinningTile = [winningTile]

data BlockState = Horizontal | Vertical | Up deriving (Eq,Ord)
--, blockStates :: BlockState , poss ::Position
data Level =  Level  { playground :: (A.Array Position Cell) , 
                       won :: Bool, 
                       lost :: Bool,
                       blockUP :: Bool,
                       blockEW :: Bool,
                       blockNS :: Bool,
                       running :: Bool,
                       activate_Cell :: (Cell,Cell),
                       activate_Pos :: (Position,Position),
                       switches :: (A.Array Position [Position]), 
                       block_position :: (Position,Position)}
    deriving (Eq, Ord)




message :: Bool -> Bool -> String
message False True = "Congrats! You won!"
message False False = "Game Over"
message True _ = ""

instance Show Level where
  show a = "\n" ++ unlines [foldl (++) "" [if x == fst (fst (block_position a) ) && y == snd (fst (block_position a)) ||
                         x == fst (snd (block_position a)) && y == snd (snd (block_position a)) 
                                then show Block 
                                  else if x == i && y == j && (running a) == False
                                    then show ((playground a) A.! (x, y)) ++ "\n" ++ message (running a) (won a)
                                    else show ((playground a) A.! (x, y)) | x <- [0..i]] | y <- [0..j]]          
                where (i, j) = snd (A.bounds (playground a))

emptyLevel :: Position -> Position -> Level
emptyLevel corner blocks  = 
    Level {playground =  (A.array ((0,0) ,(snd corner, fst corner)) [((i,j),EmptySpace) | i<- [0..snd corner], j<-[0..fst corner]]),
                                                                         won = False 
                                                                         ,lost = False
                                                                         ,blockUP = True 
                                                                         ,blockEW = False
                                                                         ,blockNS = False
                                                                         ,running = True
                                                                         ,activate_Cell = (EmptySpace,EmptySpace)
                                                                         ,activate_Pos = ((0,0),(0,0))
                                                                         ,switches = (A.array ((0,0) ,(snd corner, fst corner)) [((i,j),[]) | i<- [0..snd corner], j<-[0..fst corner]])
                                                                         ,block_position = ((snd blocks,fst blocks),(snd blocks,fst blocks))}
                                                              


addTile :: Char -> Position -> Level -> Level
addTile types tile lvl 
   
    | types == 'H'       = Level {playground = (playground lvl)A.//[((snd tile,fst tile), HardTile)] ,
                                    won = (won lvl),
                                    blockUP = (blockUP lvl),
                                    blockEW = (blockEW lvl),
                                    blockNS = (blockNS lvl),
                                    running = (running lvl),
                                    lost = (lost lvl),
                                    activate_Cell = (activate_Cell lvl),
                                    activate_Pos = (activate_Pos lvl), 
                                    switches = (switches lvl), 
                                    block_position = (block_position lvl)}

    | types == 'S'         =  Level {playground = (playground lvl)A.//[((snd tile,fst tile), SoftTile)] ,
                                    won = (won lvl),
                                    blockUP = (blockUP lvl),
                                    blockEW = (blockEW lvl),
                                    blockNS = (blockNS lvl),
                                    running = (running lvl),
                                    lost = (lost lvl) ,
                                    activate_Cell = (activate_Cell lvl),
                                    activate_Pos = (activate_Pos lvl),
                                    switches = (switches lvl),
                                    block_position = (block_position lvl)}

    | types == 'E'         =  Level {playground = (playground lvl)A.//[((snd tile,fst tile), EmptySpace)] ,
                                    won = (won lvl),
                                    blockUP = (blockUP lvl),
                                    blockEW = (blockEW lvl),
                                    blockNS = (blockNS lvl),
                                    running = (running lvl),
                                    lost = (lost lvl) ,
                                    activate_Cell = (activate_Cell lvl),
                                    activate_Pos = (activate_Pos lvl),
                                    switches = (switches lvl),
                                    block_position = (block_position lvl)}

    | otherwise            = Level {playground = (playground lvl)A.//[((snd tile,fst tile), WinningTile)] ,
                                    won = (won lvl),
                                    blockUP = (blockUP lvl),
                                    blockEW = (blockEW lvl),
                                    blockNS = (blockNS lvl),
                                    running = (running lvl),
                                    lost = (lost lvl) ,
                                    activate_Cell = (activate_Cell lvl),
                                    activate_Pos = (activate_Pos lvl),
                                    switches = (switches lvl),
                                    block_position = (block_position lvl)}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch (x_switch, y_switch) switches_list lvl = Level {playground = (playground lvl)A.//[((y_switch,x_switch), Switch)], 
                                                            won = (won lvl),
                                                            blockUP = (blockUP lvl),
                                                            blockEW = (blockEW lvl),
                                                            blockNS = (blockNS lvl),
                                                            running = (running lvl),
                                                            lost = (lost lvl), 
                                                            activate_Cell = (activate_Cell lvl),
                                                            activate_Pos = (activate_Pos lvl),
                                                            switches = (switches lvl)A.//[((y_switch,x_switch),switches_list)]  ,
                                                            block_position = (block_position lvl)}



activateSwhitch :: [Position] ->Level-> Level
activateSwhitch [] lvl = lvl
activateSwhitch (x:xs) lvl 
     | (playground lvl)A.!(snd x,fst x) == EmptySpace = activateSwhitch xs (addTile 'H' x lvl)
     | (playground lvl)A.!(snd x,fst x) == HardTile = activateSwhitch xs (addTile 'E' x lvl)
     | otherwise = lvl


activate :: Cell -> Level -> Level
activate cell_to_Check lvl 
   

    |  (fst (activate_Cell lvl)) == SoftTile && (snd (activate_Cell lvl)) == SoftTile  && (blockUP lvl)== True  =
                            Level {playground = (playground lvl), 
                                            won = False,
                                            blockUP = (blockUP lvl),
                                            blockEW = (blockEW lvl),
                                            blockNS = (blockNS lvl),
                                            running = False,
                                            lost = True,
                                            activate_Cell = (activate_Cell lvl),
                                            activate_Pos = (activate_Pos lvl),
                                            switches = (switches lvl),
                                            block_position = (block_position lvl) }

		| (fst (activate_Cell lvl)) == EmptySpace || (snd (activate_Cell lvl)) == EmptySpace = 
                            Level {playground = (playground lvl), 
                                            won = False,
                                            blockUP = (blockUP lvl),
                                            blockEW = (blockEW lvl),
                                            blockNS = (blockNS lvl),
                                            running = False,
                                            lost = True,
                                            activate_Cell = (activate_Cell lvl),
                                            activate_Pos = (activate_Pos lvl),
                                            switches = (switches lvl),
                                            block_position = (block_position lvl) }

    | (fst (activate_Cell lvl)) == WinningTile && (snd (activate_Cell lvl)) == WinningTile && (fst (activate_Pos lvl) )  == (snd (activate_Pos lvl) ) = 
                            Level {playground = (playground lvl), 
                                            won = True,
                                            blockUP = (blockUP lvl),
                                            blockEW = (blockEW lvl),
                                            blockNS = (blockNS lvl),
                                            running = False,
                                            lost = False,
                                            activate_Cell = (activate_Cell lvl),
                                            activate_Pos = (activate_Pos lvl),
                                            switches = (switches lvl),
                                            block_position = (block_position lvl) }

    | (fst (activate_Cell lvl)) == Switch =
                            Level { playground = ((playground  (activateSwhitch ((switches lvl)A.!(fst (activate_Pos lvl)) ) lvl))), 
                                            won = False,
                                            blockUP = (blockUP lvl),
                                            blockEW = (blockEW lvl),
                                            blockNS = (blockNS lvl),
                                            running = (running lvl),
                                            lost = False,
                                            activate_Cell = (activate_Cell lvl),
                                            activate_Pos = (activate_Pos lvl),
                                            switches = (switches lvl), 
                                            block_position = (block_position lvl)  }  
                                                      
    | (snd (activate_Cell lvl)) == Switch = 
                            Level { playground = ((playground  (activateSwhitch ((switches lvl)A.!(snd (activate_Pos lvl)))  lvl))), 
                                            won = False,
                                            blockUP = (blockUP lvl),
                                            blockEW = (blockEW lvl),
                                            blockNS = (blockNS lvl),
                                            running = (running lvl),
                                            lost = False,
                                            activate_Cell = (activate_Cell lvl),
                                            activate_Pos = (activate_Pos lvl),
                                            switches = (switches lvl), 
                                            block_position = (block_position lvl)  }    
    | otherwise  = lvl
    

update :: (Position,Position) ->Bool->Bool->Bool->Level-> Level
update pos blUP blEW blNS lvl = activate ((playground lvl)A.!( 0, 0))
									  Level {  playground = (playground lvl), 
                                            won = (won lvl),
                                            blockUP = blUP,
                                            blockNS = blNS,
                                            blockEW = blEW,
                                            running = (running lvl),
                                            lost = (lost lvl),
                                            activate_Cell = ((playground lvl)A.!( fst pos ),(playground lvl)A.!(snd pos) ),
                                            activate_Pos = (fst pos, snd pos),
                                            switches = (switches lvl),
                                            block_position = pos }

makeMove :: Level->Bool->Bool->Bool->Int->Int->Int->Int->Level
makeMove lvl blUP blEW blNS c1 c2 c3 c4 = 
  update ((fst (fst (block_position lvl)) + c1 ,(snd (fst (block_position lvl))) + c2) ,(fst (snd (block_position lvl)) + c3 ,(snd (snd (block_position lvl)))+ c4 )) blUP blEW blNS lvl 

  
move :: Directions -> Level -> Level
move direction lvl 
        |direction == North && ((blockUP lvl) == True) && ((blockEW lvl) == False) &&((blockNS lvl) == False) = 
            makeMove lvl False False True 0 (-1) 0  (-2)     
        | direction == South && ((blockUP lvl) == True) && ((blockEW lvl) == False) &&((blockNS lvl) == False) = 
            makeMove lvl False False True 0 2 0 1 
        | direction == West && ((blockUP lvl) == True) && ((blockEW lvl) == False) &&((blockNS lvl) == False) = 
            makeMove lvl False True False (-2) 0 (-1) 0
        | direction == East && ((blockUP lvl) == True) && ((blockEW lvl) == False) &&((blockNS lvl) == False) = 
            makeMove lvl False True False 1 0 2 0
        
        |direction == North && ((blockUP lvl) == False) && ((blockEW lvl) == True) &&((blockNS lvl) == False) = 
            makeMove lvl False True False 0 (-1) 0  (-1)
        |direction == South && ((blockUP lvl) == False) && ((blockEW lvl) == True) &&((blockNS lvl) == False) = 
            makeMove lvl False True False 0 1 0 1
        |direction == West && ((blockUP lvl) == False) && ((blockEW lvl) == True) &&((blockNS lvl) == False) = 
            makeMove lvl True False False (-1) 0 (-2) 0
        |direction == East && ((blockUP lvl) == False) && ((blockEW lvl) == True) &&((blockNS lvl) == False) = 
            makeMove lvl True False False 2 0 1 0
        
        
        |direction == West && ((blockUP lvl) == False) && ((blockEW lvl) == False) &&((blockNS lvl) == True) = 
            makeMove lvl False False True (-1) 0 (-1) 0
        |direction == East && ((blockUP lvl) == False) && ((blockEW lvl) == False) &&((blockNS lvl) == True) = 
            makeMove lvl False False True 1 0 1 0
        |direction == North && ((blockUP lvl) == False) && ((blockEW lvl) == False) &&((blockNS lvl) == True) = 
            makeMove lvl True False False 0 (-2) 0 (-1)
        |direction == South && ((blockUP lvl) == False) && ((blockEW lvl) == False) &&((blockNS lvl) == True) = 
            makeMove lvl True False False 0 1 0 2             
     	  
        | otherwise = lvl  


continueGame :: Level -> Bool
continueGame lvl   
    | (won lvl) == False && (lost lvl) == False = True
    | otherwise = False


wonGame :: Level -> Bool
wonGame lvl
    | (won lvl) == True && (running lvl) == False = True
    | otherwise = False

run :: Level -> Bool
run lvl
    | (running lvl) == True = True
    | otherwise = False

situations :: (Directions,Level) -> Bool
situations (_ , (Level _ True _ _ _ _ False _ _ _ _)) = True
situations (_ , (Level _ False _ _ _ _ False _ _  _ _)) = False
situations (_ , (Level _ _ _ _ _ _ True _ _ _ _)) = True



instance ProblemState Level Directions where
    successors l 
        | isGoal l = []
        | otherwise = filter situations $ (map (\x->(x,move x l)) [North,South,West,East])
    
    isGoal = wonGame 

