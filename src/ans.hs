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

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
  -- Vérifie si tous les éléments de la liste 'reactifs reaction' sont présents dans la liste 'sequence'
  -- et s'il n'y a aucun élément de la liste 'inhibiteurs reaction' présent dans la liste 'sequence'.
  all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))

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

-- Verifier si une liste de séquence produit une réaction
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
{-afficherTousCasLst :: Generateur -> [Reaction] -> Integer -> IO [[Sequence]]
afficherTousCasLst generateur reactions profondeur = afficherTousCasLstAux generateur reactions profondeur 1 [[]] []
  where
    afficherTousCasLstAux generateur reactions profondeur currentDepth previousRes acc
      | currentDepth >= profondeur = return acc
      | otherwise = do
          putStrLn $ "Profondeur " ++ show currentDepth ++ ": \n"
          putStrLn $ "   > Input  :" ++ show previousRes
          let currentResTemp = verifSysteme previousRes reactions -- On applique les réactions sur les séquences
          putStrLn $ "   > Output :" ++ show currentResTemp
          let currentRes = [g : res | res <- currentResTemp, g <- generateur]
          putStrLn $ "   > Leafs  :" ++ show currentRes
          let newAcc = if supDoublons acc currentRes == [] then acc else acc ++ [supDoublons acc currentRes] -- On ajoute les nouvelles séquences dans le acc en vérifiant les doublons
          putStrLn $ "   > Res    :" ++ show newAcc
          putStrLn "\n"
          if acc == newAcc
            then do
              putStrLn ("####### Stabilisation du système à la profondeur : " ++ show currentDepth)
              return acc
            else afficherTousCasLstAux generateur reactions profondeur (currentDepth + 1) currentRes newAcc-}

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

-- Fonction pour imprimer l'arbre entier de manière récursive
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

--   *********************** PROPOSITIONS ***********************

--    *********************** MAIN ***********************
main :: IO ()
main = do
  putStrLn "\n    [CHARGEMENT...]\n"
  putStrLn "\n                    ------- GENERATEUR -------\n"
  generateur <- chargerGenerateur "./data/generateur.txt"
  print generateur
  putStrLn "\n                    ------- REACTIONS -------\n"
  reactions <- chargerReactions "./data/HCC1954-ext.txt"
  mapM_ print reactions
  putStrLn "\n                 ------- ENTITES A VERIFIER -------\n"
  entites <- chargerEntites "./data/entites.txt"
  print entites
  putStrLn "\n    [TRAITEMENT...]\n"
  putStrLn "\n                 ------- CREATION DE L'ARBRE -------\n"
  result <- afficherTousCasLst generateur reactions
  putStrLn "\n                    ------- RESULT (LIST) -------\n"
  print result
  putStrLn "\n                    ------- RESULT (ARBRE) -------\n"
  afficherListeEnArbre result
  putStrLn "\n                    ------- VERIFICATION ENTITE -------\n"
  mapM_ (\entite -> print (entite, presenceEntite entite result)) entites
  putStrLn "\n    [FIN DU PROGRAMME]\n"
  putStrLn "\n"

--    *********************** TESTS ***********************

-- verifReac ["a","c","d"] (Reaction ["a"] ["b"] ["c"]) --> True
-- verifSequence [["a"],["b"]] reactionTest1 --> [["c"],[]]
-- verifSequence [["a","c"],["b","c"],["a", ""],["b", ""]] reactionTest1 --> [["c"],["d"],["c"],[]]
-- procRec ["egf"] reactionTest2 --> [["erbb1","erk12"],["p70s6k"]]
-- notreNub (cycle [1,2,3]) --> [1,2,3]
-- appliquerReactions ["a","c"] reactionTest1 --> ["c","d"]
-- appliquerReactions ["b"] reactionTest1 --> []
-- afficherTousCasLst generateurTest reactionTest1 profondeur
-- chargeAfficherReactions "reactions.txt"
-- chargeAfficherGenerateur "generateur.txt"

alphaSystem :: [Reaction]
alphaSystem =
  [ Reaction ["egf"] ["e", "p"] ["erbb1"],
    Reaction ["egf"] [] ["erk12"],
    Reaction ["erk12"] [] ["p70s6k"],
    Reaction ["a"] ["b"] ["c"],
    Reaction ["c"] ["a"] ["d"]
  ]

-- Séquences de tests

betaSequence :: [Sequence]
betaSequence =
  [ ["egf"],
    ["egf", "e"],
    ["erk12", "egf"],
    ["a", "c"]
  ]

--    *********************** CHARGEMENT ***********************

-- :l ./src/ans2.hs
-- main