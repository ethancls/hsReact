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

-- ******** FONCTIONS DE VERIFICATIONS ********

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
    all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))

-- Appliquer les réactions pour produire de nouvelles entités
applyReactionsUnique :: [Reaction] -> Sequence -> Sequence
applyReactionsUnique reactions env = foldl addUnique env [produits r | r <- reactions, verifReac env r]
  where
    addUnique = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x])

-- ******** PROCESSUS RÉCURSIF AVEC CONSTRUCTION D'UN ARBRE N-AIRE ********

-- Fonction pour générer l'arbre avec toutes les possibilités
processusRecNAire :: Sequence -> [Reaction] -> Generateur -> Int -> [Sequence] -> Arbre (Sequence, Sequence)
processusRecNAire env reactions [] _ _ = Feuille (env, env)
processusRecNAire env reactions generateur depth history
    | depth == 0 = Feuille (env, env)
    | otherwise =
        -- Appliquer les réactions à l'environnement actuel
        let envReagit = applyReactionsUnique reactions env
            newHistory = env : history
            -- Créer les sous-arbres seulement si la nouvelle séquence n'a pas déjà été traitée
            sousArbres =
                [ processusRecNAire (applyReactionsUnique reactions (envReagit ++ [e])) reactions generateur (depth - 1) newHistory
                | e <- generateur
                , notElem (envReagit ++ [e]) newHistory
                ]
         in Noeud (env, envReagit) sousArbres

-- ******** AFFICHAGE DU PROCESSUS ********

afficheArbre :: Sequence -> Arbre (Sequence, Sequence) -> Generateur -> [Reaction] -> IO ()
afficheArbre _ (Feuille _) _ _ = return () -- Ne rien afficher pour les feuilles
afficheArbre input (Noeud (env, output) enfants) (context : generateur) reactions = do
    -- Construire la liste dynamique du processus rec X. (a.X + b.X + c.X + ...)
    let processusStr = "rec X. (" ++ intercalate " + " (map (++ ".X") (context : generateur)) ++ ")"
    -- Afficher la ligne du processus
    putStrLn $ "Input: " ++ show env ++ " , proc: " ++ processusStr ++ ", context: " ++ context ++ ", output: " ++ show output
    -- Afficher les sous-arbres enfants avec l'environnement mis à jour
    case enfants of
        [] -> return () -- Handle the case where enfants is an empty list
        _ -> mapM_ (\enfant -> afficheArbre output enfant generateur reactions) enfants

-- Fonction pour imprimer l'arbre entier de manière récursive
printArbreComplet :: (Show a) => Arbre a -> Int -> IO ()
printArbreComplet (Feuille val) indent = putStrLn (replicate indent '-' ++ " Feuille: " ++ show val)
printArbreComplet (Noeud val enfants) indent = do
    putStrLn (replicate indent '-' ++ " Noeud: " ++ show val)
    putStrLn (replicate (indent + 2) '-' ++ " Enfants: ")
    mapM_ (\enfant -> printArbreComplet enfant (indent + 4)) enfants

-- ******** SYSTÈME DE TEST *********

-- Système de réactions (vous pouvez changer les réactions ici)
alphaSystem :: [Reaction]
alphaSystem =
    [ Reaction ["a"] ["b"] ["c"] -- Si "a" est présent sans "b", "c" est produit
    , Reaction ["c"] ["a"] ["d"] -- Si "c" est présent sans "a", "d" est produit
    ]

-- ******** EXEMPLE D'UTILISATION *********

main :: IO ()
main = do
    -- Charger les réactions
    let reactions = alphaSystem -- Ici, les réactions sont codées en dur pour le test

    -- Environnement initial avec des entités
    let initialEnv = [] -- Environnement vide au départ
    -- Utilisation d'un générateur simple
    let generateur = ["a", "b"] -- Générateur de nouvelles entités
    -- Construction de l'arbre avec une profondeur maximale (ici, égale à la taille du générateur)
    let arbrePossibilites = processusRecNAire initialEnv reactions generateur 3 []

    -- Affichage de l'arbre complet avec indentation
    putStrLn "===== Affichage de l'arbre complet ====="
    printArbreComplet arbrePossibilites 0

    -- Affichage du processus récursif avec contexte
    putStrLn "\n===== Suivi du processus récursif ====="
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