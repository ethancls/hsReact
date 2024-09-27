{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Monad qualified
import Data.List (intercalate)

-- Définitions des types
type Entites = String
type Generateur = [Entites]
type Sequence = [Entites]

-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- Définition d'un arbre N-aire avec suivi de l'historique
data Arbre a = Feuille a | Noeud a [Arbre a] deriving (Show)

reactionTest1 :: [Reaction]
reactionTest1 =
    [ Reaction ["a"] ["b"] ["c"] -- Si "a" est présent sans "b", "c" est produit
    , Reaction ["c"] ["a"] ["d"] -- Si "c" est présent sans "a", "d" est produit
    ]

generateurTest :: Generateur
generateurTest = ["a", "b"]

depth :: Integer
depth = 5

reactionTest2 :: [Reaction]
reactionTest2 =
    [ Reaction ["egf"] ["e", "p"] ["erbb1"]
    , Reaction ["egf"] [] ["erk12"]
    , Reaction ["erk12"] [] ["p70s6k"]
    ]

-- ******** FONCTIONS DE VERIFICATIONS ********

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
-- verifReac ["a","c","d"] (Reaction ["a"] ["b"] ["c"])
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
    all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))

-- Test des séquences sur l'ensemble des réactions
-- verifSequence [["a"],["b"]] reactionTest1 --> [["c"],[]]
-- verifSequence [["a","c"],["b","c"],["a", ""],["b", ""]] reactionTest1 --> [["c"],["d"],["c"],[]]
verifSequence :: [Sequence] -> [Reaction] -> [[Entites]]
verifSequence sequences reactions =
    [ concat [produits reaction | reaction <- reactions, verifReac sequence reaction]
    | sequence <- sequences
    ]

-- verifSequenceEach [["a"],["b"]] reactionTest1 --> [["c"],[]]
-- verifSequenceEach [["a","c"],["b","c"],["a"],["b"]] reactionTest1 --> [["c","d"],["d"],["c"],[]]
verifSequenceEach :: [Sequence] -> [Reaction] -> [[Entites]]
verifSequenceEach sequences reactions =
    [ applyReactionsUnique sequence reactions
    | sequence <- sequences
    ]

-- procRec ["egf"] reactionTest2 --> [["erbb1","erk12"],["p70s6k"]]
procRec :: Generateur -> [Reaction] -> [Sequence]
procRec gLst reactions = processusRec [] gLst reactions
  where
    processusRec :: Sequence -> Generateur -> [Reaction] -> [Sequence]
    processusRec env gLst reactions =
        let initEnv = env ++ gLst
         in notreNub (processusRecAux reactions initEnv)
      where
        processusRecAux :: [Reaction] -> Sequence -> [Sequence]
        processusRecAux reactions env =
            let newEnv = applyReactionsUnique env reactions
             in if null newEnv
                    then []
                    else newEnv : processusRecAux reactions newEnv

-- notreNub (cycle [1,2,3])
notreNub :: (Eq a) => [a] -> [a]
notreNub lst = reverse (notreNubAux lst [])
  where
    notreNubAux [] acc = acc
    notreNubAux (x : xs) acc = if x `elem` acc then notreNubAux [] acc else notreNubAux xs (x : acc)

-- Appliquer les réactions pour produire de nouvelles entités
-- applyReactionsUnique ["a","c"] reactionTest1 --> ["c","d"]
-- applyReactionsUnique ["b"] reactionTest1 --> []
applyReactionsUnique :: Sequence -> [Reaction] -> Sequence
applyReactionsUnique sequence reactions = foldl appliquerReactionsPourEntite [] sequence
  where
    appliquerReactionsPourEntite acc entite = foldl ajouterUnique acc (concatMap produitsSiActive reactions) -- Appliquer les réactions pour une entité spécifique et mettre à jour la séquence finale
      where
        produitsSiActive r = if verifReac [entite] r then produits r else []
    ajouterUnique acc x = if x `elem` acc then acc else acc ++ [x] -- Ajouter des éléments uniques à la séquence résultante

-- doTree generateurTest reactionTest1 depth
doTree :: Generateur -> [Reaction] -> Integer -> IO ()
doTree generateur reactions depth = doTreeAux generateur reactions depth 0 [] [[]]
  where
    doTreeAux generateur reactions depth currentDepth currentRes previousRes =
        Control.Monad.when (currentDepth < depth) $ do
            print previousRes
            let currentResTemp = verifSequenceEach previousRes reactions
            let currentRes = concatMap (\res -> map (\g -> g : res) generateur) currentResTemp
            doTreeAux generateur reactions depth (currentDepth + 1) [] currentRes

-- doTree generateurTest reactionTest1 depth
-- doTree :: Generateur -> [Reaction] -> Integer -> IO ()
-- doTree generateur reactions depth = doTreeAux generateur reactions depth 0 [] [[]]
--   where
--     doTreeAux generateur reactions depth currentDepth currentRes previousRes =
--         if currentDepth < depth
--             then do
--                 print previousRes
--                 let currentResTemp = verifSequenceEach previousRes reactions
--                 let currentRes = concatMap (\res -> map (\g -> g : res) generateur) currentResTemp
--                 doTreeAux generateur reactions depth (currentDepth + 1) [] currentRes
--             else return ()