module Main where

import Data.List (intersect)

-- Définition des entités et réactions
data Entity = Entity String
    deriving (Show, Eq, Ord)

data Reaction = Reaction { r :: [Entity], i :: [Entity], p :: [Entity] }
    deriving (Show, Eq)

type SetEntity = [Entity]
type SetReaction = [Reaction]

-- Conditions pour les processus
data Condition
    = Present Entity
    | Absent Entity
    | And Condition Condition
    | Or Condition Condition
    deriving (Show, Eq)

-- Processus
data Process
    = Inaction
    | Prefix SetEntity Process
    | Conditional Condition SetEntity Process Process
    | Choice Process Process
    | Parallel Process Process
    | Recursive String Process
    | Call String
    deriving (Show, Eq)

-- États du système
data State = State { process :: Process, input :: SetEntity, output :: SetEntity, ctx :: SetEntity }
    deriving (Show, Eq)

-- Store for recursive definitions
type RecStore = [(String, Process)]

-- Évaluer une condition
evalCondition :: Condition -> SetEntity -> Bool
evalCondition (Present e) input = e `elem` input
evalCondition (Absent e) input = e `notElem` input
evalCondition (And c1 c2) input = evalCondition c1 input && evalCondition c2 input
evalCondition (Or c1 c2) input = evalCondition c1 input || evalCondition c2 input

-- Évaluer une seule réaction
evalReaction :: SetEntity -> Reaction -> SetEntity
evalReaction input (Reaction r i p)
    | r `subsetOf` input && null (input `intersect` i) = p
    | otherwise = []
  where
    subsetOf xs ys = all (`elem` ys) xs

-- Évaluer un ensemble de réactions
evalReactions :: SetEntity -> SetReaction -> SetEntity
evalReactions input reactions = foldl (\acc r -> acc ++ evalReaction input r) [] reactions

-- Évaluer un processus avec la gestion des appels récursifs
evalProcess :: SetEntity -> Process -> RecStore -> Process
evalProcess input Inaction _ = Inaction
evalProcess input (Prefix _ p) store = evalProcess input p store
evalProcess input (Conditional cond _ p1 p2) store
    | evalCondition cond input = evalProcess input p1 store
    | otherwise = evalProcess input p2 store
evalProcess input (Choice p1 p2) store
    | evalProcess input p1 store == Inaction = evalProcess input p2 store
    | otherwise = evalProcess input p1 store
evalProcess input (Parallel p1 p2) store = Parallel (evalProcess input p1 store) (evalProcess input p2 store)
evalProcess input (Recursive name p) store = evalProcess input (Call name) ((name, p) : store)
evalProcess input (Call name) store = case lookup name store of
    Just p -> evalProcess input p store
    Nothing -> Inaction

-- Transition entre états
transition :: State -> SetReaction -> RecStore -> State
transition (State p input output ctx) reactions store =
    let newOutput = evalReactions (ctx ++ input) reactions
    in State (evalProcess input p store) input newOutput ctx

-- Exemple d'utilisation dans le main
main :: IO ()
main = do
    -- Définition des entités
    let a = Entity "A"
    let b = Entity "B"
    let c = Entity "C"
    let d = Entity "D"

    -- Définition des réactions
    let reactions = [Reaction [a] [] [c], Reaction [b] [] [d]]

    -- Définition du processus récursif
    let recProcess = Recursive "X" (Choice (Prefix [a] (Call "X")) (Prefix [b] (Call "X")))

    -- État initial avec input vide, process récursif et contexte a
    let initialState = State recProcess [] [] [a]

    -- Store pour les appels récursifs
    let recStore = [("X", Choice (Prefix [a] (Call "X")) (Prefix [b] (Call "X")))]

    -- Afficher l'état initial
    putStrLn "État initial:"
    print initialState

    -- Transition 1 : Input vide, contexte a, output devrait être c
    let nextState1 = transition initialState reactions recStore
    putStrLn "\nÉtat après la première transition:"
    print nextState1

    -- Transition 2 : Input c, contexte b, output devrait être d
    let nextState2 = transition (State recProcess [c] [] [b]) reactions recStore
    putStrLn "\nÉtat après la deuxième transition:"
    print nextState2

    -- Transition 3 : Input d, contexte a, output devrait être c
    let nextState3 = transition (State recProcess [d] [] [a]) reactions recStore
    putStrLn "\nÉtat après la troisième transition:"
    print nextState3

    -- Transition 4 : Input c, contexte b, output devrait être d
    let finalState = transition (State recProcess [c] [] [b]) reactions recStore
    putStrLn "\nÉtat après la quatrième transition:"
    print finalState