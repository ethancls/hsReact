
{-

--   *********************** SYSTÈME DE RÉACTION ***********************

\$ E.NICOLAS 12100466 ethan.bento-nicolas@edu.univ-paris13.fr
\$ D.PALAHIN 12106973 dmytro.palahin@edu.univ-paris13.fr

-}

--    *********************** IMPORTS ***********************

import Control.Monad (when)
import Data.List (intercalate, nub, sort)
import Data.List.NonEmpty (nubBy)

--    *********************** TYPES ***********************

type Entites = String
type Generateur = [Entites]
type Sequence = [Entites]

--    *********************** DATA ***********************

-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- Définition d'un arbre N-aire
data Arbre a = Feuille a | Noeud a [Arbre a] deriving (Show)

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
    putStrLn "\n                    ------- GENERATEUR -------\n"
    print generateur
    putStrLn "\n"

chargeAfficherEntites :: FilePath -> IO ()
chargeAfficherEntites chemin = do
    generateur <- chargerGenerateur chemin
    putStrLn "\n                 ------- ENTITES A VERIFIER -------\n"
    print generateur
    putStrLn "\n"

--    *********************** PROCESSUS ***********************

afficherTousCasLst :: Generateur -> [Reaction] -> Integer -> IO [[Sequence]]
afficherTousCasLst generateur reactions profondeur = afficherTousCasLstAux generateur reactions profondeur 0 [[]] []
    where
        afficherTousCasLstAux generateur reactions profondeur currentDepth previousRes acc
                | currentDepth >= profondeur = return acc
                | otherwise = do
                        putStrLn $ "Profondeur " ++ show currentDepth ++ ": \n"
                        putStrLn $ "   > Input  :" ++ show previousRes
                        let currentResTemp = verifSequenceEach previousRes reactions
                        putStrLn $ "   > Output :" ++ show currentResTemp
                        let currentRes = [g : res | res <- currentResTemp, g <- generateur]
                        putStrLn $ "   > Leafs  :" ++ show currentRes
                        let newAcc = if removeDuplicates acc currentRes == [] then acc else acc ++ [removeDuplicates acc currentRes]
                        putStrLn $ "   > Acc    :" ++ show newAcc
                        putStrLn "\n"
                        if acc == newAcc
                            then do
                                putStrLn ("####### Stabilisation du RS à la profondeur : " ++ show currentDepth)
                                return acc
                            else afficherTousCasLstAux generateur reactions profondeur (currentDepth + 1) currentRes newAcc

-- Fonction pour éliminer les doublons à tous les niveaux de profondeur
removeDuplicates :: (Eq a) => [[[a]]] -> [[a]] -> [[a]]
removeDuplicates acc = filter (`notElem` concat acc)

verifEntite :: Entites -> [[[Entites]]] -> Bool
verifEntite entite = any (any (elem entite))

afficherTousCasArbre :: Generateur -> [Reaction] -> Integer -> IO (Arbre (Sequence, [Sequence]))
afficherTousCasArbre generateur reactions profondeur = afficherTousCasArbreAux generateur reactions profondeur 0 [] []
    where
        afficherTousCasArbreAux :: Generateur -> [Reaction] -> Integer -> Integer -> Sequence -> [Sequence] -> IO (Arbre (Sequence, [Sequence]))
        afficherTousCasArbreAux generateur reactions profondeur currentDepth previousRes acc
            | currentDepth >= profondeur = return $ Feuille (previousRes, acc)
            | otherwise = do
                putStrLn $ "Profondeur " ++ show currentDepth ++ ": \n"
                putStrLn $ "   > Input :" ++ show previousRes
                let currentResTemp = verifSequenceEach [previousRes] reactions -- This should return [Sequence], not [[Sequence]]
                putStrLn $ "   > Output :" ++ show currentResTemp
                let currentRes = [g : res | res <- currentResTemp, g <- generateur] -- Ensure this doesn't over-nest
                putStrLn $ "   > Leafs :" ++ show currentRes
                let newAcc = if concat currentResTemp `elem` acc then acc else acc ++ currentRes
                putStrLn $ "   > Acc :" ++ show newAcc
                putStrLn "\n"
                sousArbres <- mapM (\res -> afficherTousCasArbreAux generateur reactions profondeur (currentDepth + 1) res newAcc) currentRes
                return $ Noeud (previousRes, currentRes) sousArbres


--    ************************** ARBRE ****************************

-- Fonction pour générer l'arbre avec toutes les possibilités
processusRecNAire :: Sequence -> [Reaction] -> Generateur -> Int -> [Sequence] -> Arbre (Sequence, Sequence)
processusRecNAire env reactions generateur depth history
    | depth == 0 = Feuille (env, env)
    | otherwise =
        let envReagit = appliquerReactionsUnique env reactions
            newHistory = env : history
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
    putStrLn "\n    [CHARGEMENT...]\n"
    chargeAfficherReactions "reactions.txt"
    chargeAfficherGenerateur "generateur.txt"
    chargeAfficherEntites "entites.txt"
    putStrLn "\n    [TRAITEMENT...]\n"
    let profondeur = 10
    generateur <- chargerGenerateur "generateur.txt"
    reactions <- chargerReactions "reactions.txt"
    entites <- chargerGenerateur "entites.txt"
    let arbre = processusRecNAire [] reactions generateur (fromIntegral profondeur) []
    putStrLn "\n                 ------- CREATION DE L'ARBRE -------\n"
    putStrLn ("PROFONDEUR MAX : " ++ show profondeur ++ "\n")
    result <- afficherTousCasLst generateur reactions profondeur
    putStrLn "\n                    ------- RESULT (LIST) -------\n"
    print result
    putStrLn "\n                    ------- RESULT (ARBRE) -------\n"
  {-   printArbreComplet arbre 0 -}
    putStrLn "\n                    ------- VERIFICATION ENTITE -------\n"
    mapM_ (\entite -> print (entite, verifEntite entite result)) entites
    putStrLn "\n    [FIN DU PROGRAMME]\n"
    putStrLn "\n"

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