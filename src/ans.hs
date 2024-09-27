import Data.List ( intercalate, nub )

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
applyReactionsUnique reactions env = nub $ concat [produits r | r <- reactions, verifReac env r]

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
            sousArbres = [processusRecNAire (applyReactionsUnique reactions (envReagit ++ [e])) reactions generateur (depth - 1) newHistory
                            | e <- generateur, (envReagit ++ [e]) `notElem` newHistory]
        in Noeud (env, envReagit) sousArbres

-- ******** AFFICHAGE DU PROCESSUS ********

{- afficheArbre :: Sequence -> Arbre (Sequence, Sequence) -> Generateur -> [Reaction] -> IO ()
afficheArbre _ (Feuille _) _ _ = return ()  -- Ne rien afficher pour les feuilles
afficheArbre input (Noeud (env, output) enfants) (context:generateur) reactions = do
    -- Construire la liste dynamique du processus rec X. (a.X + b.X + c.X + ...)
    let processusStr = "rec X. (" ++ intercalate " + " (map (++ ".X") (context:generateur)) ++ ")"
    -- Appliquer les réactions pour produire de nouvelles entités
    let newOutput = applyReactionsUnique reactions (env ++ [context])
    -- Afficher la ligne du processus
    putStrLn $ "Input: " ++ show env ++ " , proc: " ++ processusStr ++ ", context: " ++ context ++ ", output: " ++ show newOutput
    -- Afficher les sous-arbres enfants avec l'environnement mis à jour
    case enfants of
        [] -> return ()  -- Handle the case where enfants is an empty list
        [enfant@(Noeud _ _)] -> afficheArbre newOutput enfant generateur reactions  -- Handle the case where enfants is a single Noeud
        [enfant@(Feuille _)] -> afficheArbre newOutput enfant generateur reactions  -- Handle the case where enfants is a single Feuille
        _  -> mapM_ (\enfant -> afficheArbre newOutput enfant generateur reactions) enfants
afficheArbre _ (Noeud _ enfants) [] reactions = do
    -- Afficher les sous-arbres enfants avec l'environnement mis à jour
    case enfants of
        [] -> return ()  -- Handle the case where enfants is an empty list
        [enfant@(Noeud _ _)] -> afficheArbre [] enfant [] reactions  -- Handle the case where enfants is a single Noeud
        [enfant@(Feuille _)] -> afficheArbre [] enfant [] reactions  -- Handle the case where enfants is a single Feuille
        _  -> mapM_ (\enfant -> afficheArbre [] enfant [] reactions) enfants
 -}


-- ******** SYSTÈME DE TEST *********

-- Système de réactions (vous pouvez changer les réactions ici)
alphaSystem :: [Reaction]
alphaSystem =
    [ Reaction ["a"] ["b"] ["c"]  -- Si "a" est présent sans "b", "c" est produit
    , Reaction ["c"] ["a"] ["d"]  -- Si "c" est présent sans "a", "d" est produit
    ]

-- ******** EXEMPLE D'UTILISATION *********


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