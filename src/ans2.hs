{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (intercalate)

-- Définitions des types
type Entites = String
type Generateur = [Entites]
type Sequence = [Entites]

-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- Définition d'un arbre N-aire avec suivi de l'historique
data Arbre a = Feuille a | Noeud a [Arbre a] deriving (Show)

reaction1 :: [Reaction]
reaction1 =
    [ Reaction ["a"] ["b"] ["c"] -- Si "a" est présent sans "b", "c" est produit
    , Reaction ["c"] ["a"] ["d"] -- Si "c" est présent sans "a", "d" est produit
    ]

reaction2 :: [Reaction]
reaction2 =
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

-- procRec ["egf"] reaction2
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
-- applyReactionsUnique ["a","c"] reaction1 --> ["c","d"]
-- applyReactionsUnique ["b"] reaction1 --> []
applyReactionsUnique :: Sequence -> [Reaction] -> Sequence
applyReactionsUnique sequence reactions = foldl appliquerReactionsPourEntite [] sequence
  where
    appliquerReactionsPourEntite acc entite = foldl ajouterUnique acc (concatMap produitsSiActive reactions) -- Appliquer les réactions pour une entité spécifique et mettre à jour la séquence finale
      where
        produitsSiActive r = if verifReac [entite] r then produits r else []
    ajouterUnique acc x = if x `elem` acc then acc else acc ++ [x] -- Ajouter des éléments uniques à la séquence résultante
