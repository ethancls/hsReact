{-# OPTIONS_GHC -Wno-x-partial #-}

--    *********************** IMPORTS ***********************
import Control.Monad (when)
import Data.List (intercalate, nub)

--    *********************** TYPES ***********************
type Entites = String
type Generateur = [Entites]
type Sequence = [Entites]

--    *********************** DATA ***********************
-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- Définition d'un arbre N-aire avec suivi de l'historique
data Arbre a = Feuille a | Noeud a [Arbre a] deriving (Show)

--    *********************** VARIABLES ***********************
reactionTest1 :: [Reaction]
reactionTest1 =
    [ Reaction ["a"] ["b"] ["c"] -- Si "a" est présent sans "b", "c" est produit
    , Reaction ["c"] ["a"] ["d"] -- Si "c" est présent sans "a", "d" est produit
    ]

generateurTest :: Generateur
generateurTest = ["a", "b"]

profondeur :: Integer
profondeur = 5

reactionTest2 :: [Reaction]
reactionTest2 =
    [ Reaction ["egf"] ["e", "p"] ["erbb1"]
    , Reaction ["egf"] [] ["erk12"]
    , Reaction ["erk12"] [] ["p70s6k"]
    ]

--    *********************** FONCTIONS DE VÉRIFICATIONS ***********************
-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
    all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))

-- Test des séquences sur l'ensemble des réactions
verifSequence :: [Sequence] -> [Reaction] -> [[Entites]]
verifSequence sequences reactions =
    [ concat [produits reaction | reaction <- reactions, verifReac sequence reaction]
    | sequence <- sequences
    ]

verifSequenceEach :: [Sequence] -> [Reaction] -> [[Entites]]
verifSequenceEach sequences reactions =
    map (`appliquerReactionsUnique` reactions) sequences

--    *********************** FONCTIONS ***********************
procRec :: Generateur -> [Reaction] -> [Sequence]
procRec gLst reactions = processusRec [] gLst reactions
  where
    processusRec :: Sequence -> Generateur -> [Reaction] -> [Sequence]
    processusRec env gLst reactions =
        let initEnv = env ++ gLst
         in nub (processusRecAux reactions initEnv)
      where
        processusRecAux :: [Reaction] -> Sequence -> [Sequence]
        processusRecAux reactions env =
            let newEnv = appliquerReactionsUnique env reactions
             in if null newEnv
                    then []
                    else newEnv : processusRecAux reactions newEnv

-- Appliquer les réactions pour produire de nouvelles entités
appliquerReactionsUnique :: Sequence -> [Reaction] -> Sequence
appliquerReactionsUnique sequence reactions = foldl appliquerReactionsPourEntite [] sequence
  where
    appliquerReactionsPourEntite acc entite = foldl ajouterUnique acc (concatMap produitsSiActive reactions)
      where
        produitsSiActive r = if verifReac [entite] r then produits r else []
    ajouterUnique acc x = if x `elem` acc then acc else acc ++ [x]

--    *********************** FONCTIONS DE CHARGEMENT ***********************
-- Fonction pour découper une chaîne en fonction d'un séparateur
decouperPar :: Char -> String -> [String]
decouperPar separateur = foldr (\c l -> if c == separateur then [] : l else (c : head l) : tail l) [[]]

-- Fonction pour parser une ligne de réaction du fichier
parserReaction :: String -> Reaction
parserReaction str =
    let parties = decouperPar ';' str
        reactifs = decouperPar ',' (head parties)
        inhibiteurs = decouperPar ',' (parties !! 1)
        produits = decouperPar ',' (parties !! 2)
     in Reaction reactifs inhibiteurs produits

-- Fonction pour charger les réactions depuis un fichier
chargerReactions :: FilePath -> IO [Reaction]
chargerReactions chemin = do
    contenu <- readFile chemin
    return $ map parserReaction (lines contenu)

chargeAfficherReactions :: FilePath -> IO ()
chargeAfficherReactions chemin = do
    reactions <- chargerReactions chemin
    putStrLn "\n                    ------- REACTIONS -------\n"
    mapM_ print reactions -- Afficher chaque réaction sur une nouvelle ligne
    putStrLn "\n"

parserGenerateur :: String -> Generateur
parserGenerateur = decouperPar ','

chargerGenerateur :: FilePath -> IO Generateur
chargerGenerateur chemin = do
    contenu <- readFile chemin
    return $ parserGenerateur (head (lines contenu))

chargeAfficherGenerateur :: FilePath -> IO ()
chargeAfficherGenerateur chemin = do
    generateur <- chargerGenerateur chemin
    putStrLn "\n                    ------- GENERATEUR -------"
    print generateur
    putStrLn "\n"

--    *********************** AFFICHAGE DU PROCESSUS ***********************
afficherTousCasLst :: Generateur -> [Reaction] -> Integer -> IO ()
afficherTousCasLst generateur reactions profondeur = afficherTousCasLstAux generateur reactions profondeur 0 [[]]
  where
    afficherTousCasLstAux generateur reactions profondeur currentDepth previousRes =
        when (currentDepth < profondeur) $ do
            print previousRes
            let currentResTemp = verifSequenceEach previousRes reactions
            let currentRes = [g : res | res <- currentResTemp, g <- generateur]
            afficherTousCasLstAux generateur reactions profondeur (currentDepth + 1) currentRes

-- Fonction pour générer l'arbre avec toutes les possibilités
processusRecNAire :: Sequence -> [Reaction] -> Generateur -> Int -> [Sequence] -> Arbre (Sequence, Sequence)
processusRecNAire env reactions [] _ _ = Feuille (env, env)
processusRecNAire env reactions generateur depth history
    | depth == 0 = Feuille (env, env)
    | otherwise =
        -- Appliquer les réactions à l'environnement actuel
        let envReagit = appliquerReactionsUnique env reactions
            newHistory = env : history
            -- Créer les sous-arbres seulement si la nouvelle séquence n'a pas déjà été traitée
            sousArbres =
                [ processusRecNAire (appliquerReactionsUnique (envReagit ++ [e]) reactions) reactions generateur (depth - 1) newHistory
                | e <- generateur
                , notElem (envReagit ++ [e]) newHistory
                ]
         in Noeud (env, envReagit) sousArbres

-- Fonction pour imprimer l'arbre entier de manière récursive
printArbreComplet :: (Show a) => Arbre a -> Int -> IO ()
printArbreComplet (Feuille val) indent = putStrLn (replicate indent '-' ++ " Feuille: " ++ show val)
printArbreComplet (Noeud val enfants) indent = do
    putStrLn (replicate indent '-' ++ " Noeud: " ++ show val)
    putStrLn (replicate (indent + 2) '-' ++ " Enfants: ")
    mapM_ (\enfant -> printArbreComplet enfant (indent + 4)) enfants

--    *********************** MAIN ***********************
main :: IO ()
main = do
    chargeAfficherReactions "reactions.txt"
    chargeAfficherGenerateur "generateur.txt"
    let profondeur = 4
    generateur <- chargerGenerateur "generateur.txt"
    reactions <- chargerReactions "reactions.txt"
    let arbre = processusRecNAire [] reactions generateur (fromIntegral profondeur) []
    putStrLn "\n                    ------- RESULT (LISTS) -------\n"
    afficherTousCasLst generateur reactions profondeur
    putStrLn "\n                    ------- RESULT (ARBRE) -------\n"
    printArbreComplet arbre 0
    putStrLn "\n\n"

--    *********************** TESTS ***********************
-- verifReac ["a","c","d"] (Reaction ["a"] ["b"] ["c"]) --> True
-- verifSequence [["a"],["b"]] reactionTest1 --> [["c"],[]]
-- verifSequence [["a","c"],["b","c"],["a", ""],["b", ""]] reactionTest1 --> [["c"],["d"],["c"],[]]
-- verifSequenceEach [["a"],["b"]] reactionTest1 --> [["c"],[]]
-- verifSequenceEach [["a","c"],["b","c"],["a"],["b"]] reactionTest1 --> [["c","d"],["d"],["c"],[]]
-- procRec ["egf"] reactionTest2 --> [["erbb1","erk12"],["p70s6k"]]
-- notreNub (cycle [1,2,3]) --> [1,2,3]
-- appliquerReactionsUnique ["a","c"] reactionTest1 --> ["c","d"]
-- appliquerReactionsUnique ["b"] reactionTest1 --> []
-- afficherTousCasLst generateurTest reactionTest1 profondeur
-- chargeAfficherReactions "reactions.txt"
-- chargeAfficherGenerateur "generateur.txt"

--    *********************** CHARGEMENT ***********************
-- :l ./src/ans2.hs
-- main