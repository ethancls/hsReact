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
    addUnique acc' p = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) acc' p

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