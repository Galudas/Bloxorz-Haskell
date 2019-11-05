{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe

import qualified Data.Set as S

data Node s a = Node{
    
    state :: s ,
    actions :: Maybe a,
    parent :: Maybe (Node s a),
    height :: Int,
    children :: [(a,s)] 

} | Null 
                



nodeState :: Node s a -> s
nodeState n = (state n)

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace s = Node {
    
    state = s,
    actions =  Nothing,
    parent = Nothing,
    height = 0,
    children = (successors s)
}

instance Eq s => Eq (Node s a) where
   (==) (Node s1 _ _ _ _ ) (Node s2 _ _ _ _ ) = (s1 == s2) 
 
instance Ord s => Ord (Node s a) where
   compare (Node s1 _ _ _ _ ) (Node s2 _ _ _ _ ) = (compare s1  s2)
{-
    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}
  
helperLimitedDfs start depth mySet = 

    foldl (\mySet x -> if ((S.member (snd x) (fst mySet)) || depth == 0) 
        then mySet
        else helperLimitedDfs  Node { state = (snd x) , actions = (Just (fst x)), parent = (Just start), height = ((height start) + 1), children = (successors (snd x))  } (depth - 1) 
        (S.insert (snd x) (fst mySet), Node { state =  (snd x) , actions = (Just (fst x)), parent = (Just start) , height = ((height start) + 1), children =  (successors (snd x))} : (snd mySet)))
    
    mySet
    
    (children start)



limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs start height = (reverse (snd (helperLimitedDfs start height (S.insert (state start)(S.empty) , [start]))))

{-
 

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

  -- = let ceva = (issWin list_Nodes) in
  --   if (state (fst ceva)) == Null then ceva else  iterativeDeepeningHelper start_node (height + 1) (limitedDfs start_node (height + 1))

iterativeDeepeningHelper :: (ProblemState s a, Ord s)
    => Node s a     
    -> Int
    -> Int
    -> [Node s a]
    -> (Node s a, Int)

iterativeDeepeningHelper start_node heights nr [] = iterativeDeepeningHelper start_node (heights + 1) nr (limitedDfs start_node  (heights + 1))
iterativeDeepeningHelper start_node heights nr (x:xs) = if (isGoal (state x)) == True then  (x, nr) else iterativeDeepeningHelper start_node  heights (nr + 1) xs

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening start_node = iterativeDeepeningHelper start_node 0 0 (limitedDfs start_node 0)
    

{-

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.
    
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
extractPathHelper :: Node s a -> [(a,s)] -> [(a,s)]
extractPathHelper start_node toReturn 
    | (isNothing (parent start_node)) == True = toReturn
    | otherwise = extractPathHelper (fromJust (parent start_node)) [((fromJust (actions  start_node)),state  start_node)]++toReturn 
 
extractPath :: Node s a -> [(a, s)]
extractPath start_node = extractPathHelper start_node []

{-
    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve j b = (extractPath (fst (iterativeDeepening  (createStateSpace j))))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
