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