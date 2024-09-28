<<<<<<< HEAD
{-# OPTIONS_GHC -Wno-x-partial #-}

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
-- :l ./src/ans.hs
-- main
=======
{-

    ******************* SYSTÈME DE RÉACTION ******************

\$ E.NICOLAS 12100466 ethan.bento-nicolas@edu.univ-paris13.fr
\$ D.PALAHIN 12106973 dmytro.palahin@edu.univ-paris13.fr

-}

--    *********************** IMPORTS ***********************

import Data.List (nub)

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

-- Fonction pour vérifier si un inhibiteur est présent dans une liste de réactifs
contientInhibiteurs :: [String] -> [String] -> Bool
contientInhibiteurs reactifs = any (`elem` reactifs)

-- Fonction qui vérifie si une entité peut réagir
verifEntite :: String -> Sequence -> [Reaction] -> Sequence
verifEntite entite sequence reactions =
  -- Chercher une réaction valide pour l'entité, en comparant avec toute la séquence (réactifs et inhibiteurs)
  concat [produits r | r <- reactions, entite `elem` reactifs r, not (contientInhibiteurs sequence (inhibiteurs r))]

-- Vérifier chaque entité dans la séquence pour voir si elle peut réagir
verifSequence :: Sequence -> [Reaction] -> Sequence
verifSequence sequence reactions = nub $ concatMap (\entite -> verifEntite entite sequence reactions) sequence

-- Verifier une liste de séquence 
verifSysteme :: [Sequence] -> [Reaction] -> [Sequence]
verifSysteme sequences reactions =
  [verifSequence sequence reactions | sequence <- sequences]

--    *********************** FONCTIONS PROCESSUS SIMPLE ***********************

-- Fonction pour générer tous les produits possibles de la réaction en chaine d'un generateur
-- Exemple :
-- recUnique ["egf"] alphaSystem
-- [["erbb1","erk12"],["p70s6k"]]
-- egf crée erbb1 et erk12 puis erk12 crée p70s6k
reacChaine :: Generateur -> [Reaction] -> [Sequence]
reacChaine g reactions = processusRec [] g reactions
  where
    processusRec env g reactions =
      let initEnv = env ++ g
       in nub (processusRecAux reactions initEnv)
      where
        processusRecAux reactions env =
          let newEnv = verifSequence env reactions
           in if null newEnv || newEnv == env
                then []
                else newEnv : processusRecAux reactions newEnv

--    *********************** PROCESSUS REC ***********************

-- Fonction qui génère les séquences possibles via des listes
afficherTousCasLst :: [Generateur] -> [Reaction] -> IO [[Sequence]]
afficherTousCasLst generateur reactions = afficherTousCasLstAux generateur reactions 1 [[]] []
  where
    afficherTousCasLstAux generateur reactions currentDepth previousRes acc = do
      putStrLn $ "Profondeur " ++ show currentDepth ++ ": \n"
      putStrLn $ "   > Input  :" ++ show previousRes
      let currentResTemp = verifSysteme previousRes reactions -- On applique les réactions sur les séquences
      putStrLn $ "   > Output :" ++ show currentResTemp
      let currentRes = [g ++ res | res <- currentResTemp, g <- generateur]
      putStrLn $ "   > Leafs  :" ++ show currentRes
      let newAcc = if supDoublons acc currentRes == [] then acc else acc ++ [supDoublons acc currentRes] -- On ajoute les nouvelles séquences dans le acc en vérifiant les doublons
      putStrLn $ "   > Res    :" ++ show newAcc
      putStrLn "\n"
      if acc == newAcc
        then do
          putStrLn ("####### Stabilisation du système à la profondeur : " ++ show currentDepth)
          return acc
        else afficherTousCasLstAux generateur reactions (currentDepth + 1) currentRes newAcc

-- Fonction pour éliminer les doublons dans acc
supDoublons :: (Eq a) => [[[a]]] -> [[a]] -> [[a]]
supDoublons acc = filter (`notElem` concat acc)

-- Fonction pour vérifier si une entité est présente
presenceEntite :: Entites -> [[[Entites]]] -> Bool
presenceEntite entite = any (any (elem entite))

--    ************************** ARBRE ****************************

-- Fonction pour imprimer l'arbre entier
printArbreComplet :: (Show a) => Arbre a -> Int -> IO ()
printArbreComplet (Feuille val) indent = putStrLn (replicate indent '-' ++ " Feuille: " ++ show val)
printArbreComplet (Noeud val enfants) indent = do
  putStrLn (replicate indent '-' ++ " ETAT_SYS [" ++ show indent ++ "] : " ++ show val)
  mapM_ (\enfant -> printArbreComplet enfant (indent + 1)) enfants

-- Fonction pour convertir la liste de séquences en arbre
convertirEnArbre :: [[Sequence]] -> Arbre [Sequence]
convertirEnArbre [] = Feuille []
convertirEnArbre (x : xs) = Noeud x (map convertirEnArbre (groupByDepth xs))

-- Fonction pour grouper les séquences par profondeur
groupByDepth :: [[Sequence]] -> [[[Sequence]]]
groupByDepth [] = []
groupByDepth xs =
  let (first, rest) = span ((== head (map length xs)) . length) xs
   in first : groupByDepth rest

-- Fonction pour afficher la liste de afficherTousCasLst sous forme d'arbre
afficherListeEnArbre :: [[Sequence]] -> IO ()
afficherListeEnArbre sequences = do
  let arbre = convertirEnArbre sequences
  printArbreComplet arbre 1

--    *********************** FONCTIONS DE CHARGEMENT FICHIERS ***********************

-- Fonction pour découper une chaîne en fonction d'un séparateur
split :: Char -> String -> [String]
split separateur = foldr (\c l -> if c == separateur then [] : l else (c : head l) : tail l) [[]]

-- Fonction pour parser une ligne de réaction du fichier
parserReaction :: String -> Reaction
parserReaction entree =
    let str = filter (/= '\r') entree -- Supprimer les caractères Windows
        parties = split ';' str
        reactifs = split ',' (head parties)
        inhibiteurs = split ',' (parties !! 1)
        produits = split ',' (parties !! 2)
     in Reaction reactifs inhibiteurs produits

-- Fonction pour charger les réactions depuis un fichier
chargerReactions :: FilePath -> IO [Reaction]
chargerReactions chemin = do
    contenu <- readFile chemin
    return $ map parserReaction (lines contenu)

-- Fonction pour charger le générateur depuis un fichier
chargerGenerateur :: FilePath -> IO [Generateur]
chargerGenerateur chemin = do
  contenu <- readFile chemin
  return $ map (split ',') (split ';' contenu)

-- Fonction pour charger les entités à vérifier depuis un fichier
chargerEntites :: FilePath -> IO [Entites]
chargerEntites chemin = do
  contenu <- readFile chemin
  return $ split ',' (head (lines contenu))

--   *********************** PROPOSITIONS LOGIQUES ***********************

-- Expression Phi
data Phi
  = Var Entites
  | Not Phi
  | And Phi Phi
  | Or Phi Phi
  deriving (Show, Eq)

-- Evaluer une expression Phi
eval :: Phi -> (Entites -> Bool) -> Bool
eval (Var x) f = f x
eval (Not e) f = not (eval e f)
eval (And e1 e2) f = eval e1 f && eval e2 f
eval (Or e1 e2) f = eval e1 f || eval e2 f

-- Tester une proposition sur une séquence
testProp :: Phi -> Sequence -> Bool
testProp prop sequence = eval prop (`elem` sequence)

-- Fonction pour parser une chaîne en une Phi
parsePhi :: String -> Phi
parsePhi str = 
  let tokens = words str
   in parseTokens tokens

-- Fonction auxiliaire pour parser
parseTokens :: [String] -> Phi
parseTokens [x] = Var x
parseTokens ("!":xs) = Not (parseTokens xs)
parseTokens (x:"^":xs) = And (Var x) (parseTokens xs)
parseTokens (x:"v":xs) = Or (Var x) (parseTokens xs)
parseTokens _ = error "Invalid format"

-- Fonction pour vérifier si une entité est éventuellement produite dans une séquence
eventually :: Phi -> [[Sequence]] -> Bool
eventually phi = any (any (testProp phi))

-- Fonction pour vérifier si une entité est toujours produite dans une séquence
always :: Phi -> [[Sequence]] -> Bool
always phi = all (all (testProp phi))

-- Fonction pour vérifier si une entité est produite jusqu'à ce qu'une autre entité soit produite
untilP :: Phi -> Phi -> [[Sequence]] -> Bool
untilP phi1 phi2 sequences = 
  let hold = eventually phi1 sequences
      remain = eventually phi2 sequences
   in hold && remain

--    *********************** MAIN ***********************
hsreact :: IO ()
hsreact = do

  putStrLn ("\n                  ██╗  ██╗███████╗██████╗ ███████╗ █████╗  ██████╗████████╗\n" ++
            "                  ██║  ██║██╔════╝██╔══██╗██╔════╝██╔══██╗██╔════╝╚══██╔══╝\n" ++
            "                  ███████║███████╗██████╔╝█████╗  ███████║██║        ██║   \n" ++
            "                  ██╔══██║╚════██║██╔══██╗██╔══╝  ██╔══██║██║        ██║   \n" ++
            "                  ██║  ██║███████║██║  ██║███████╗██║  ██║╚██████╗   ██║   \n" ++
            "                  ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝ ╚═════╝   ╚═╝   \n" ++
            "\n > SUP GALILEE - UNIVERSITE PARIS 13 - G4SI2 - PROJET SYSTEME DE REACTION\n" ++
            "   > ETHAN NICOLAS & DMYTRO PALAHIN\n" ++
            "    > 2024\n")
  putStrLn "\n    [CHARGEMENT...]\n"

  putStrLn "Utiliser les fichiers de test par défaut ? (y/n)"
  reponse <- getLine

  (generateur, reactions, entites) <- 
    if reponse == "n"
      then do
        putStrLn "Entrez le chemin du fichier de générateur :"
        cheminGenerateur <- getLine
        putStrLn "Entrez le chemin du fichier de réactions :"
        cheminReactions <- getLine
        putStrLn "Entrez le chemin du fichier d'entités à vérifier :"
        cheminEntites <- getLine
        generateur <- chargerGenerateur cheminGenerateur
        reactions <- chargerReactions cheminReactions
        entites <- chargerEntites cheminEntites
        putStrLn "Fichiers chargés avec succès !"
        return (generateur, reactions, entites)
      else do
        generateur <- chargerGenerateur "./data/generateur.txt"
        reactions <- chargerReactions "./data/HCC1954.txt"
        entites <- chargerEntites "./data/entites.txt"
        putStrLn "Fichiers chargés avec succès !"
        return (generateur, reactions, entites)

  
  putStrLn "Afficher les donnees chargees ? (y/n)"
  reponse <- getLine
  if reponse == "y"
    then do
      putStrLn "\n                    ------- GENERATEUR -------\n"
      print generateur
      putStrLn "\n                    ------- REACTIONS -------\n"
      mapM_ print reactions
      putStrLn "\n                 ------- ENTITES A VERIFIER -------\n"
      print entites
    else do
      return ()

  putStrLn "\n    [TRAITEMENT...]\n"

  putStrLn "\n                ------- CREATION DE L'ARBRE -------\n"

  result <- afficherTousCasLst generateur reactions

  putStrLn "Afficher les etats ? (y/n)"
  reponse <- getLine
  if reponse == "y"
    then do
      putStrLn "\n                  ------- RESULTAT (LISTE) -------\n"
      print result
      putStrLn "\n                 ------- RESULTAT (ETAPES) -------\n"
      afficherListeEnArbre result
    else do
      return ()

  putStrLn "\n               ------- VERIFICATION ENTITE -------\n"

  mapM_ (\entite -> print (entite, presenceEntite entite result)) entites

  putStrLn "\n                ------- VERIFICATION PHI -------\n"

  let phi = "(! akt)^(! e)"
  putStrLn $ "Proposition : " ++ show phi
  print $ parsePhi phi
  putStrLn "Il y a au moins un etat au cours de l'execution qui verifie la proposition"
  print $ eventually (parsePhi phi) result 
  putStrLn "Tous les etats au cours de l'execution verifient la proposition"
  print $ always (parsePhi phi) result
  putStrLn "On a au moisn un p70s6k jusqu'a ce que l'entite mtor soit produite"
  print $ untilP (parsePhi "p70s6k") (parsePhi "mtor") result
  
  putStrLn "\n    [FIN DU PROGRAMME]\n"

--    *********************** TESTS ***********************

alphaSystem :: [Reaction]
alphaSystem =
  [ Reaction ["egf"] ["e", "p"] ["erbb1"],
    Reaction ["egf"] [] ["erk12"],
    Reaction ["erk12"] [] ["p70s6k"],
    Reaction ["a"] ["b"] ["c"],
    Reaction ["c"] ["a"] ["d"]
  ]

betaSequence :: [Sequence]
betaSequence =
  [ ["egf"],
    ["egf", "e"],
    ["erk12", "egf"],
    ["a", "c"]
  ]

--  FIN DU PROGRAMME
>>>>>>> main
