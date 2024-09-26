import Data.List (nub)
import System.Random (randomRIO)

-- ******** DÉFINITIONS DE TYPES ********

-- Définitions des types
type Entites = String
type Sequence = [Entites]

-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- ******** FONCTIONS DE VERIFICATIONS ********

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
    all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))

-- Appliquer les réactions et maintenir des produits uniques
applyReactionsUnique :: [Reaction] -> Sequence -> Sequence
applyReactionsUnique reactions env = nub (concat [produits r | r <- reactions, verifReac env r])

-- ******** GÉNÉRATION D'ENTITÉS *********

-- Générer une entité aléatoire ("a" ou "b")
genEntite :: IO Entites
genEntite = do
    idx <- randomRIO (0, 1)  -- Générer un indice pour "a" ou "b"
    return [toEnum (fromEnum 'a' + idx)]

-- Générer une séquence d'entités aléatoires de taille donnée
genEntites :: Int -> IO Sequence
genEntites 0 = return []
genEntites n = do
    entite <- genEntite
    rest <- genEntites (n - 1)
    return (entite : rest)

-- ******** LECTURE DES RÉACTIONS À PARTIR D'UN FICHIER *********

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

-- ******** PROCESSUS AVEC ITÉRATIONS *********

-- Fonction récursive pour simuler des interactions jusqu'à une boucle ou la production d'une entité spécifique
recK :: Sequence -> Sequence -> [Reaction] -> Int -> String -> IO ()
recK env context reactions maxIterations targetEntite = processusAux env context reactions 0
    where
        processusAux :: Sequence -> Sequence -> [Reaction] -> Int -> IO ()
        processusAux currentEnv currentContext rs iteration
            | iteration >= maxIterations = putStrLn "Nombre maximum d'itérations atteint."
            | targetEntite `elem` currentEnv = putStrLn $ "L'entité " ++ targetEntite ++ " a été produite !"
            | otherwise = do
                -- Ajouter le contexte à l'environnement actuel
                let updatedEnv = nub (currentEnv ++ currentContext)
                
                -- Appliquer les réactions et récupérer les nouveaux produits
                let newEnv = applyReactionsUnique rs updatedEnv
                
                -- Si aucun nouveau produit n'est généré, cela signifie qu'il n'y a pas de nouvelles réactions
                if null newEnv
                then putStrLn "Aucun nouveau produit généré, fin du processus."
                else do
                    putStrLn $ "Iteration " ++ show iteration ++ ": " ++ show newEnv
                    
                    -- Recur avec l'environnement mis à jour et uniquement les produits comme nouvel environnement
                    newContext <- genEntites 1 -- Générer un nouveau contexte aléatoire
                    processusAux newEnv newContext rs (iteration + 1)

-- ******** EXEMPLE D'UTILISATION *********

main :: IO ()
main = do
    -- Charger les réactions depuis un fichier texte
    reactions <- loadReactions "RS.txt"

    putStrLn "Réactions chargées :"
    print reactions
    
    -- Initialiser une séquence vide
    let initialSeq = []

    -- Définir une entité cible pour l'arrêt
    let targetEntite = "d"

    putStrLn "Entité cible :"
    print targetEntite

    -- Lancer le processus aléatoire avec un nombre maximum d'itérations
    recK initialSeq initialSeq reactions 10 targetEntite