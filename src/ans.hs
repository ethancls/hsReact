<<<<<<< HEAD
{-

\**** REACTION SYSTEMS ****

\$ E.NICOLAS 12100466 ethan.bento-nicolas@edu.univ-paris13.fr
\$ D.PALAHIN 12106973 dmytro.palahin@edu.univ-paris13.fr

Simple project for testing systems under string
format with sequences of entities or with process
on the environment.

We can also define a system as a set of logical propositions i.e P = {φ1 = e1 ∧¬e2, ...}

-}

-- ******* DEFINITIONS DES TYPES | STRUCTURES *******
=======
import Data.List (intercalate)
>>>>>>> main

-- Définitions des types
type Entites = String
type Generateur = [Entites]
type Sequence = [Entites]

-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- Définition d'un arbre N-aire avec suivi de l'historique
data Arbre a = Feuille a | Noeud a [Arbre a] deriving (Show)

-- ******** FONCTIONS DE VERIFICATIONS ********

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
    all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))
<<<<<<< HEAD

-- Test des séquences sur l'ensemble des réactions
verifSequence :: [Sequence] -> [Reaction] -> [[Entites]]
verifSequence sequences reactions =
    [ concat [produits reaction | reaction <- reactions, verifReac sequence reaction]
    | sequence <- sequences
    ]

-- Verification de présence d'une entité
verifEntite :: Entites -> [Sequence] -> [Reaction] -> Bool
verifEntite entite sequences reactions =
    any (elem entite) (verifSequence sequences reactions)

-- ********* SYSTEMES DE TESTS *********

-- Système de réactions

alphaSystem :: [Reaction]
alphaSystem =
    [ Reaction ["egf"] ["e", "p"] ["erbb1"]
    , Reaction ["egf"] [] ["erk12"]
    , Reaction ["erk12"] [] ["p70s6k"]
    ]

-- Séquences de tests

betaSequence :: [Sequence]
betaSequence =
    [ ["egf"]
    , ["egf", "e"]
    , ["erk12"]
    , ["p"]
    ]

-- ******** PROCESSUS D'ENVIRONNEMENT *********

{-
Fonction de processus : fonction qui pars de empty,
ajoute ce qu'elle veut a l'env et produit une sortie,
on reutilise la sortie dans l'environnement t+1 et on
réapplique les regles avec encore une entite aleatoire
fournie par la fonction
-}

type Generateur = [Entites] -- Rec.X (a.X + b.X) -> la fonction ajoute a ou b recursivement a la liste des entites presentes a (t - 1)

{- processus :: [Entites] -> Generateur -> [Reaction] -> [Entites]
processus = loop state return
processus e g r = (verifSequence g(e) r)  g  r -}

-- processus [] "egf" alphaSystem
processus :: Sequence -> String -> [Reaction] -> [Sequence]
-- processus env g reactions = takeWhileDifferent (iterate (applyReactionsUnique reactions) (env ++ [g]))
processus env g reactions = notreNub (iterate (applyReactionsUnique reactions) (env ++ [g]))

-- processusRec [] "egf" alphaSystem
processusRec :: Sequence -> Generateur -> [Reaction] -> [Sequence]
processusRec env gLst reactions =
    let initEnv = env ++ gLst
     in notreNub (processusRecAux reactions initEnv)
  where
    processusRecAux :: [Reaction] -> Sequence -> [Sequence]
    processusRecAux reactions env =
        let newEnv = applyReactionsUnique reactions env
         in if null newEnv
                then []
                else newEnv : processusRecAux reactions newEnv

-- applyReactionsUnique alphaSystem ["egf", "erk12"]
-- avec [] nous aurons seulement de nouveaux produits
=======

-- Appliquer les réactions pour produire de nouvelles entités
>>>>>>> main
applyReactionsUnique :: [Reaction] -> Sequence -> Sequence
applyReactionsUnique reactions env = foldl addUnique [] [produits r | r <- reactions, verifReac env r]
  where
    addUnique acc' p = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) acc' p

<<<<<<< HEAD
takeWhileDifferent :: (Eq a) => [a] -> [a]
takeWhileDifferent (x : y : xs)
    | x == y = [x]
    | otherwise = x : takeWhileDifferent (y : xs)
takeWhileDifferent xs = xs

-- notreNub (cycle [1,2,3])
notreNub :: (Eq a) => [a] -> [a]
notreNub lst = reverse (notreNubAux lst [])
  where
    notreNubAux [] acc = acc
    notreNubAux (x : xs) acc = if x `elem` acc then notreNubAux [] acc else notreNubAux xs (x : acc)

-- ******** Vérification du produit d'entité *********

{-
Vérifier si une entité donnée est produite dans un
RS lors de l'interaction avec un processus K
-}

testProcess :: Sequence
testProcess = ["a", "b"]

testReactions :: [Reaction]
testReactions =
    [ Reaction ["a"] ["b"] ["c"]
    , Reaction ["c"] ["a"] ["d"]
    ]

-- Processus K, appliquant récursivement les réactions et ajoutant des entités
-- processusKRec testReactions testProcess
processusKRec :: [Reaction] -> Sequence -> [Sequence]
processusKRec reactions processus = processusKRecAux [] reactions processus
  where
    processusKRecAux :: Sequence -> [Reaction] -> Sequence -> [Sequence]
    processusKRecAux env reactions [] = []
    processusKRecAux env reactions (p : ps) =
        let envMisAJour = applyReactionsUnique reactions (env ++ [p])
            newEnv = env ++ envMisAJour
         in if null envMisAJour
                then []
                else envMisAJour : processusKRecAux newEnv reactions ps

-- ******** TODO *********
=======
-- ******** PROCESSUS RÉCURSIF AVEC CONSTRUCTION D'UN ARBRE N-AIRE ********

-- Fonction pour générer l'arbre avec toutes les possibilités
processusRecNAire :: Sequence -> [Reaction] -> Generateur -> Int -> [Sequence] -> Arbre Sequence
processusRecNAire env reactions [] _ _ = Feuille env  -- Si le générateur est vide, on arrête ici
processusRecNAire env reactions generateur depth history
    | depth == 0 = Feuille env  -- Si la profondeur maximale est atteinte, on arrête
    | otherwise =
        -- Appliquer les réactions à l'environnement actuel
        let envReagit = applyReactionsUnique reactions env
            newHistory = env : history  -- Ajouter l'environnement courant à l'historique
        -- Créer un nœud avec les sous-arbres générés en ajoutant chaque entité du générateur
        in Noeud envReagit [processusRecNAire (applyReactionsUnique reactions (envReagit ++ [e])) reactions (filter (/= e) generateur) (depth - 1) newHistory | e <- generateur]

-- ******** AFFICHAGE DU PROCESSUS ********
>>>>>>> main

-- Fonction pour afficher l'arbre avec le contexte
afficheArbre :: Sequence -> Arbre Sequence -> Generateur -> [Reaction] -> IO ()
afficheArbre _ (Feuille _) _ _ = return ()  -- Ne rien afficher pour les feuilles
afficheArbre input (Noeud sequences enfants) (context:generateur) reactions = do
    -- Ajouter le contexte à l'environnement
    let updatedEnv = input ++ [context]
    -- Appliquer les réactions immédiatement après avoir mis à jour l'environnement
    let envAfterReactions = foldl (\acc r -> if verifReac updatedEnv r then acc ++ produits r else acc) [] reactions
    let nextOutput = case envAfterReactions of
                        [] -> "empty"  -- Si l'input est vide, "empty"
                        _ -> intercalate ", " envAfterReactions  -- Sinon, afficher les entités séparées par une virgule
    -- Construire la liste dynamique du processus rec X. (a.X + b.X + c.X + ...)
    let processusStr = "rec X. (" ++ intercalate " + " (map (++ ".X") (context:generateur)) ++ ")"
    -- Afficher la ligne du processus
    putStrLn $ "Input: " ++ show input ++ " , proc: " ++ processusStr ++ ", context: " ++ context ++ ", output: " ++ nextOutput
    -- Afficher les sous-arbres enfants avec l'environnement mis à jour
    mapM_ (\enfant -> afficheArbre envAfterReactions enfant generateur reactions) enfants


-- ******** SYSTÈME DE TEST *********

-- Système de réactions (vous pouvez changer les réactions ici)
alphaSystem :: [Reaction]
alphaSystem =
    [ Reaction ["a"] ["b"] ["c"]  -- Si "a" est présent sans "b", "c" est produit
    , Reaction ["c"] ["a"] ["d"]  -- Si "c" est présent sans "a", "d" est produit
    ]

-- ******** EXEMPLE D'UTILISATION *********

main :: IO ()
main = do
    -- Charger les réactions
    let reactions = alphaSystem  -- Ici, les réactions sont codées en dur pour le test

    -- Environnement initial avec des entités
    let initialEnv = []  -- Environnement vide au départ
    -- Utilisation d'un générateur simple
    let generateur = ["a", "b"]  -- Générateur de nouvelles entités
    -- Construction de l'arbre avec une profondeur maximale (ici, égale à la taille du générateur)
    let arbrePossibilites = processusRecNAire initialEnv reactions generateur (length generateur) []

    -- Affichage de l'arbre des possibilités avec contexte
    afficheArbre initialEnv arbrePossibilites generateur reactions

-- ******** FONCTION POUR LIRE LES REACTIONS D'UN FICHIER *********

-- Fonction pour découper une chaîne en fonction d'un séparateur
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr (\c l -> if c == delimiter then [] : l else (c : head l) : tail l) [[]]

-- Fonction pour parser une ligne de réaction du fichier
parseReaction :: String -> Reaction
parseReaction str =
    let parts = splitBy ';' str
        reactifs = splitBy ',' (head parts)
        inhibiteurs = splitBy ',' (parts !! 1)
        produits = splitBy ',' (parts !! 2)
    in Reaction reactifs inhibiteurs produits

-- Fonction pour charger les réactions depuis un fichier
loadReactions :: FilePath -> IO [Reaction]
loadReactions path = do
    contents <- readFile path
    return $ map parseReaction (lines contents)