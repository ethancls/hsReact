{-

    ******************* 🧪 SYSTÈME DE RÉACTION 🧪 ******************

\$ E.NICOLAS 12100466 ethan.bento-nicolas@edu.univ-paris13.fr
\$ D.PALAHIN 12106973 dmytro.palahin@edu.univ-paris13.fr

-}

--    *********************** IMPORTS ***********************

import Control.Exception (IOException, catch, try)
import Control.Monad (when)
import Data.Char (isAlphaNum, isSpace)
import Data.List (nub, tails, transpose)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)

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

-- Fonction pour vérifier la présence de cycles dans une liste de séquences
detectCycle :: (Eq a) => [a] -> Bool
detectCycle xs = any (\(a, b) -> a == b) [(x, y) | (x : ys) <- tails xs, y <- ys]

-- Fonction principale pour vérifier la stabilisation du RS
verifStab :: [Generateur] -> [Reaction] -> IO ()
verifStab generateur reactions = do
    result <- recK generateur reactions
    let sequences = concat result
    let stabilise = detectCycle sequences
    if stabilise
        then putStrLn "Le système se stabilise."
        else putStrLn "Le système ne se stabilise pas."

-- *********************** FONCTION POUR VERIFIER UNE LISTE DE SEQUENCE Ci ***********************

-- Verifier une liste de séquences Ci
-- Exemple :
-- ghci> verifSysteme betaSequence alphaSystem
-- [["erbb1","erk12"],["erk12"],["p70s6k","erbb1","erk12"],["c"]]

verifSysteme :: [Sequence] -> [Reaction] -> [Sequence]
verifSysteme sequences reactions =
    [verifSequence sequence reactions | sequence <- sequences]

--    *********************** FONCTIONS PROCESSUS SIMPLE ***********************

-- Fonction pour générer tous les produits possibles de la réaction en chaine d'un generateur
-- Exemple :
-- reacChaine ["egf"] alphaSystem
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
recK :: [Generateur] -> [Reaction] -> IO [[Sequence]]
recK generateur reactions = recKAux generateur reactions 1 [[]] []
  where
    recKAux generateur reactions currentDepth previousRes acc = do
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
                putStrLn ("🎉😎👍👌 Stabilisation du système à la profondeur : " ++ show currentDepth)
                return acc
            else recKAux generateur reactions (currentDepth + 1) currentRes newAcc

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
convertirEnArbre (x : xs) = Noeud x (map convertirEnArbre (grouperParProfondeur xs))

-- Fonction pour grouper les séquences par profondeur
grouperParProfondeur :: [[Sequence]] -> [[[Sequence]]]
grouperParProfondeur [] = []
grouperParProfondeur xs =
    let (premier, reste) = span ((== head (map length xs)) . length) xs
     in premier : grouperParProfondeur reste

-- Fonction pour afficher la liste de recK sous forme d'arbre
afficherListeEnArbre :: [[Sequence]] -> IO ()
afficherListeEnArbre sequences = do
    let arbre = convertirEnArbre sequences
    printArbreComplet arbre 1

--    *********************** FONCTIONS DE CHARGEMENT FICHIERS ***********************

-- Fonction pour découper une chaîne en fonction d'un séparateur
diviser :: Char -> String -> [String]
diviser separateur = foldr (\c l -> if c == separateur then [] : l else (c : head l) : tail l) [[]]

-- Fonction pour parser une ligne de réaction du fichier
parserReaction :: String -> Reaction
parserReaction entree =
    let str = filter (/= '\r') entree -- Supprimer les caractères Windows
        parties = diviser ';' str
        reactifs = diviser ',' (head parties)
        inhibiteurs = diviser ',' (parties !! 1)
        produits = diviser ',' (parties !! 2)
     in Reaction reactifs inhibiteurs produits

-- Fonction pour charger les réactions depuis un fichier

chargerReactions :: FilePath -> IO [Reaction]
chargerReactions path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> do
            putStrLn "\n❌Erreur de lecture du fichier de réactions --> aucun fichier ou répertoire de ce nom !"
            putStrLn "Veuillez entrer un chemin valide pour le fichier de réactions:"
            newPath <- myGetLine
            chargerReactions newPath -- Recursive call to retry with a new path
        Right content -> return $ map parserReaction (lines content)

-- Fonction pour charger le générateur depuis un fichier
chargerGenerateur :: FilePath -> IO [Generateur]
chargerGenerateur path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> do
            putStrLn "\n❌Erreur de lecture du fichier de générateur --> aucun fichier ou répertoire de ce nom !"
            putStrLn "Veuillez entrer un chemin valide pour le fichier de générateur:"
            newPath <- myGetLine
            chargerGenerateur newPath -- Recursive call to retry with a new path
        Right content -> return $ map (diviser ',') (diviser ';' content)

-- Fonction pour charger les entités à vérifier depuis un fichier
chargerEntites :: FilePath -> IO [Entites]
chargerEntites path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> do
            putStrLn "\n❌Erreur de lecture du fichier d'entités --> aucun fichier ou répertoire de ce nom !"
            putStrLn "Veuillez entrer un chemin valide pour le fichier d'entités :"
            newPath <- myGetLine
            chargerEntites newPath -- Recursive call to retry with a new path
        Right content -> return $ diviser ',' (head (lines content))

--    *********************** FONCTION DE LIRE LIGNE AVEC DELETE **********************

-- Custom getLine function that handles backspace and carriage return
myGetLine :: IO String
myGetLine = do
    hSetEcho stdin False -- Désactiver l'écho
    hSetBuffering stdin NoBuffering -- Désactiver le buffering
    boucle ""
  where
    boucle :: String -> IO String
    boucle str = do
        char <- getChar
        case char of
            '\n' -> do
                -- Touche Entrée
                putChar '\n'
                return str
            '\DEL' -> do
                -- Gérer la suppression/retour arrière
                when (not (null str)) $ do
                    putStr "\b \b" -- Reculer, écraser avec un espace, puis reculer à nouveau
                boucle (if null str then str else init str) -- Supprimer le dernier caractère
            _ -> do
                putChar char
                boucle (str ++ [char])

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
    let tokens = tokeniser (filter (not . isSpace) str)
     in fst (parserExpr tokens)

-- Fonction pour diviser la chaîne en tokens manuellement
tokeniser :: String -> [String]
tokeniser [] = []
tokeniser (c : cs)
    | c `elem` "^v()" = [c] : tokeniser cs
    | c == '!' = ["!"] ++ tokeniser cs
    | isAlphaNum c = let (var, rest) = span isAlphaNum (c : cs) in var : tokeniser rest
    | otherwise = tokeniser cs

-- Fonction auxiliaire pour parser
parserExpr :: [String] -> (Phi, [String])
parserExpr [] = error "❌ Format invalide"
parserExpr tokens = parserOu tokens

-- Fonction pour parser les opérateurs OR
parserOu :: [String] -> (Phi, [String])
parserOu tokens =
    let (term, rest) = parserEt tokens
     in case rest of
            ("v" : xs) ->
                let (nextTerm, rest') = parserOu xs
                 in (Or term nextTerm, rest')
            _ -> (term, rest)

-- Fonction pour parser les opérateurs AND
parserEt :: [String] -> (Phi, [String])
parserEt tokens =
    let (term, rest) = parserNon tokens
     in case rest of
            ("^" : xs) ->
                let (nextTerm, rest') = parserEt xs
                 in (And term nextTerm, rest')
            _ -> (term, rest)

-- Fonction pour parser les opérateurs NOT
parserNon :: [String] -> (Phi, [String])
parserNon ("!" : xs) =
    let (term, rest) = parserTerme xs
     in (Not term, rest)
parserNon tokens = parserTerme tokens

-- Fonction auxiliaire pour parser un terme
parserTerme :: [String] -> (Phi, [String])
parserTerme [] = error "❌ Format invalide"
parserTerme ("(" : xs) =
    let (e, rest) = parserExpr xs
     in case rest of
            (")" : rest') -> (e, rest')
            _ -> error "❌ Parenthèses non appariées"
parserTerme (x : xs)
    | isAlphaNum (head x) = (Var x, xs)
    | otherwise = error "❌ Format invalide"

-- Fonction pour vérifier si une entité est éventuellement produite dans une séquence
eventually :: Phi -> [[Sequence]] -> Bool
eventually phi = any (any (testProp phi))

-- Fonction pour vérifier si une entité est toujours produite dans une séquence
always :: Phi -> [[Sequence]] -> Bool
always phi = all (all (testProp phi))

-- Fonction pour vérifier si une entité est produite jusqu'à ce qu'une autre entité soit produite
untilP :: Phi -> Phi -> [[Sequence]] -> Bool
untilP phi1 phi2 sequences =
    let hold = always phi1 sequences
        remain = eventually phi2 sequences
     in hold && remain

-- eventually always e :
-- cela signifie qu'il existe un état dans la séquence où, à partir de cet état,
-- la propriété e est toujours vraie
eventuallyAlways :: Phi -> [[Sequence]] -> Bool
eventuallyAlways phi = any (all (testProp phi))

-- always eventually e :
-- cela signifie que pour chaque état dans la séquence, il existe un état futur où la propriété e est vraie
alwaysEventually :: Phi -> [[Sequence]] -> Bool
alwaysEventually phi = all (any (testProp phi))

--    *********************** FONCTIONS D'AFFICHAGE ***********************

afficherModif :: Bool -> IO ()
afficherModif True = putStrLn "✅ Vrai"
afficherModif False = putStrLn "❌ Faux"

-- Utilisation des résultats de TP2 pour afficher les réactions

-- En-têtes des colonnes pour les réactions
entetesReactions :: [String]
entetesReactions = ["Reactifs", "Inhibiteurs", "Produits"]

-- Fonction pour obtenir la longueur des chaînes
obtenirLongueurs :: [String] -> [Int]
obtenirLongueurs = map length

-- Fonction pour obtenir la longueur des chaînes dans une liste 2D
obtenirLongueurs2D :: [[String]] -> [[Int]]
obtenirLongueurs2D = map obtenirLongueurs

-- Fonction pour obtenir le maximum d'une liste générique
maxListeGen :: (Ord a) => [a] -> a
maxListeGen (x : xs) = foldl (\acc x -> max x acc) x xs

-- Fonction pour obtenir les longueurs maximales des colonnes
maxLongueursColonnes :: [[String]] -> [Int]
maxLongueursColonnes lst = map maxListeGen (transpose (obtenirLongueurs2D lst))

-- Fonction pour convertir une réaction en une liste de chaînes
reactionEnListe :: Reaction -> [String]
reactionEnListe (Reaction reactifs inhibiteurs produits) =
    [unwords reactifs, unwords inhibiteurs, unwords produits]

-- Fonction pour ajouter des espaces pour l'alignement
ajouterEspaces :: String -> Int -> String -> String
ajouterEspaces str n pos
    | pos == "gauche" = str ++ replicate delta ' '
    | pos == "droite" = replicate delta ' ' ++ str
    | pos == "centre" = replicate nbGauche ' ' ++ str ++ replicate nbDroite ' '
    | otherwise = error "Argument de position invalide! Utilisez 'gauche', 'droite' ou 'centre'"
  where
    delta = n - length str
    nbGauche = delta `div` 2
    nbDroite = delta - nbGauche

-- Fonction pour formater l'en-tête
formaterEntete :: [String] -> [Int] -> String
formaterEntete entetes longueurs = concat ["| " ++ ajouterEspaces entete longueur "centre" ++ " " | (entete, longueur) <- zip entetes longueurs] ++ "|"

-- Fonction pour formater une ligne
formaterLigne :: [String] -> [Int] -> String
formaterLigne ligne longueurs = concat ["| " ++ ajouterEspaces champ longueur "centre" ++ if i == length ligne - 1 then "" else " " | (champ, longueur, i) <- zip3 ligne longueurs [0 ..]] ++ " |\n"

-- Fonction pour formater les informations
formaterInfos :: [[String]] -> [Int] -> String
formaterInfos infos longueurs = concatMap (`formaterLigne` longueurs) infos

-- Fonction pour générer une ligne de tirets
genererLigneTirets :: Int -> String
genererLigneTirets n = " " ++ replicate n '-' ++ " "

-- Fonction pour générer les séparateurs
genererSeparateurs :: [Int] -> String
genererSeparateurs lst = "| " ++ foldl (\acc x -> acc ++ replicate x '-' ++ " + ") [] (init lst) ++ replicate (last lst) '-' ++ " |"

-- Fonction pour formater les réactions
afficherReactions :: [Reaction] -> IO ()
afficherReactions reactions =
    let donneesReactions = map reactionEnListe reactions
        longueursMax = maxLongueursColonnes (entetesReactions : donneesReactions)
        longueurTotale = length (formaterEntete entetesReactions longueursMax) - 1
     in do
            putStrLn (genererLigneTirets longueurTotale)
            putStrLn (formaterEntete entetesReactions longueursMax)
            putStrLn (genererSeparateurs longueursMax)
            putStr (formaterInfos donneesReactions longueursMax)
            putStrLn (genererLigneTirets longueurTotale)
            putStrLn ""

--    *********************** MAIN ***********************

hsreact :: IO ()
hsreact = do
    putStrLn
        ( "\n                  ██╗  ██╗███████╗██████╗ ███████╗ █████╗  ██████╗████████╗\n"
            ++ "                  ██║  ██║██╔════╝██╔══██╗██╔════╝██╔══██╗██╔════╝╚══██╔══╝\n"
            ++ "                  ███████║███████╗██████╔╝█████╗  ███████║██║        ██║   \n"
            ++ "                  ██╔══██║╚════██║██╔══██╗██╔══╝  ██╔══██║██║        ██║   \n"
            ++ "                  ██║  ██║███████║██║  ██║███████╗██║  ██║╚██████╗   ██║   \n"
            ++ "                  ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝ ╚═════╝   ╚═╝   \n"
            ++ "\n > 🎯🎯 SUP GALILEE - UNIVERSITE PARIS 13 - G4SI2 - 🔥PROJET SYSTEME DE REACTION🔥\n"
            ++ "   > 🥷 ETHAN NICOLAS & 🥷 DMYTRO PALAHIN\n"
            ++ "    > ⏰ 2024\n"
        )
    putStrLn "\n    [⏳⏳ CHARGEMENT...]\n"

    let demanderFichiers = do
            putStrLn "Utiliser les fichiers de test par défaut ? (y/n)"
            reponse <- myGetLine
            case reponse of
                "y" -> do
                    let askForFileChoice = do
                            putStrLn "\nNous avons les fichiers de réactions suivants :"
                            putStrLn "1. HCC1954.txt"
                            putStrLn "2. HCC1954-ext.txt"
                            putStrLn "3. BT474.txt"
                            putStrLn "4. BT474-ext.txt"
                            putStrLn "5. SKBR3.txt"
                            putStrLn "6. SKBR3-ext.txt"
                            putStr "\nSélectionnez un fichier de réactions en entrant le numéro correspondant : "
                            choix <- myGetLine
                            case choix of
                                "1" -> return "./data/HCC1954.txt"
                                "2" -> return "./data/HCC1954-ext.txt"
                                "3" -> return "./data/BT474.txt"
                                "4" -> return "./data/BT474-ext.txt"
                                "5" -> return "./data/SKBR3.txt"
                                "6" -> return "./data/SKBR3-ext.txt"
                                _ -> do
                                    putStrLn "\n❌ Numéro invalide ! Veuillez entrer un numéro entre 1 et 6."
                                    askForFileChoice
                    fichierReactions <- askForFileChoice
                    putStrLn $ "\nUtilisation du fichier : " ++ fichierReactions
                    generateur <- chargerGenerateur "./data/generateur.txt"
                    reactions <- chargerReactions fichierReactions
                    entites <- chargerEntites "./data/entites.txt"
                    putStrLn "\nFichiers chargés avec succès 🎉🎉🎊🎊"
                    return (generateur, reactions, entites)
                "n" -> do
                    putStrLn "\nExample de la chemin vers votre fichier est : ./data/fichier.txt"
                    putStrLn "\nEntrez le chemin du fichier de générateur :"
                    cheminGenerateur <- myGetLine
                    generateur <- chargerGenerateur cheminGenerateur
                    putStrLn "\nEntrez le chemin du fichier de réactions :"
                    cheminReactions <- myGetLine
                    reactions <- chargerReactions cheminReactions
                    putStrLn "\nEntrez le chemin du fichier d'entités à vérifier :"
                    cheminEntites <- myGetLine
                    entites <- chargerEntites cheminEntites
                    putStrLn "Fichiers chargés avec succès !"
                    return (generateur, reactions, entites)
                _ -> do
                    putStrLn "❌ Réponse invalide ! Veuillez répondre par 'y' ou 'n'.\n\n"
                    demanderFichiers

    (generateur, reactions, entites) <- demanderFichiers

    let demanderDonnes = do
            putStrLn "\n\nAfficher les donnees chargees ? (y/n)"
            reponse <- myGetLine
            case reponse of
                "y" -> do
                    putStrLn "\n                    ------- 🚀 GENERATEUR -------\n"
                    print generateur
                    putStrLn "\n                    ------- 🧪 REACTIONS -------\n"
                    -- mapM_ print reactions
                    afficherReactions reactions
                    putStrLn "\n                 ------- 🔍 ENTITES A VERIFIER -------\n"
                    print entites
                "n" -> return ()
                _ -> do
                    putStrLn "❌ Réponse invalide ! Veuillez répondre par 'y' ou 'n'."
                    demanderDonnes
    demanderDonnes

    putStrLn "\n    [💪🦾 TRAITEMENT...]\n"

    putStrLn "\n                ------- 🌳🌳 CREATION DE L'ARBRE -------\n"

    result <- recK generateur reactions

    let demanderEtat = do
            putStrLn "\n\nAfficher les états ? (y/n)"
            reponse <- myGetLine
            case reponse of
                "y" -> do
                    putStrLn "\n                  ------- 📋 RESULTAT (LISTE) -------\n"
                    print result
                    putStrLn "\n                 ------- 📊 RESULTAT (ETAPES) -------\n"
                    afficherListeEnArbre result
                "n" -> return ()
                _ -> do
                    putStrLn "❌ Réponse invalide ! Veuillez répondre par 'y' ou 'n'."
                    demanderEtat
    demanderEtat

    putStrLn "\n               ------- 🔍 VERIFICATION ENTITE -------\n"

    let maxLength = maximum (map length entites)

    mapM_
        ( \entite ->
            let presence = presenceEntite entite result
                message =
                    if presence
                        then "✅ est présente"
                        else "🚫 n'est pas présente"
                paddedEntite = entite ++ replicate (maxLength - length entite) ' '
             in putStrLn $ paddedEntite ++ " --> " ++ message
        )
        entites

    putStrLn "\n                ------- 🔍 VERIFICATION PHI -------\n"

    let demanderPhi = do
            putStrLn "Utiliser le fichier pour expression de phi par défaut ? (y/n)"
            reponsePhi <- myGetLine
            case reponsePhi of
                "y" -> do
                    putStrLn "\nUtilisation du fichier : ./data/phi.txt\n"
                    content <- readFile "./data/phi.txt"
                    return (head (lines content))
                "n" -> do
                    examplePhi <- readFile "./data/phi.txt"
                    putStrLn $ "Exemple d'expression pour phi (tiré de ./data/phi.txt) : " ++ head (lines examplePhi)
                    putStrLn "Entrez votre expression pour phi :"
                    myGetLine
                _ -> do
                    putStrLn "❌ Réponse invalide ! Veuillez répondre par 'y' ou 'n'.\n\n"
                    demanderPhi

    phi <- demanderPhi
    putStrLn $ "🔍👀 Proposition        : " ++ show phi
    putStrLn $ "✨👀 Proposition parsée : " ++ show (parsePhi phi)
    putStrLn "\n👉 Il y a au moins un état au cours de l'execution qui vérifie la proposition > ◇φ (👀 eventually)"
    afficherModif (eventually (parsePhi phi) result)
    putStrLn "\n👉 Tous les états au cours de l'execution vérifient la proposition > □φ (💡 always)"
    afficherModif (always (parsePhi phi) result)
    putStrLn "\n🎯 Maintenant, nous voulons vérifier la proposition > φUφ' (🔍 until)"
    putStr "Entrez votre expression pour φ  (par exemple, egf) : "
    phi <- myGetLine
    putStr "Entrez votre expression pour φ' (par exemple, p)   : "
    phi_prim <- myGetLine
    putStrLn ("\n👉 On a " ++ show phi ++ " jusqu'a ce que l'entite " ++ show phi_prim ++ " soit produite")
    afficherModif (untilP (parsePhi phi) (parsePhi phi_prim) result)

    putStrLn "\n🎯 Maintenant, nous voulons vérifier la proposition imbriquée > ◇□e 👀💡 et > □◇e 💡👀"
    putStr "Entrez votre expression pour (par exemple, egf) : "
    expression <- myGetLine
    putStrLn ("\n👉 Proposition : ◇□" ++ show expression ++ " représente qu'il existe un état dans la séquence où, à partir de cet état, la propriété " ++ show expression ++ " est toujours vraie")
    afficherModif (eventuallyAlways (parsePhi expression) result)
    putStrLn ("\n👉 Proposition : □◇" ++ show expression ++ " signifie que pour chaque état dans la séquence, il existe un état futur où la propriété " ++ show expression ++ " est vraie")
    afficherModif (alwaysEventually (parsePhi expression) result)
    putStrLn "\n\n    [🎬🎬 FIN DU PROGRAMME]\n\n\n"

--    *********************** TESTS ***********************

alphaSystem :: [Reaction]
alphaSystem =
    [ Reaction ["egf"] ["e", "p"] ["erbb1"]
    , Reaction ["egf"] [] ["erk12"]
    , Reaction ["erk12"] [] ["p70s6k"]
    , Reaction ["a"] ["b"] ["c"]
    , Reaction ["c"] ["a"] ["d"]
    ]

betaSequence :: [Sequence]
betaSequence =
    [ ["egf"]
    , ["egf", "e"]
    , ["erk12", "egf"]
    , ["a", "c"]
    ]

--  FIN DU PROGRAMME