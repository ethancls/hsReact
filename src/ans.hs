import Data.Char (GeneralCategory)
import Data.Void (Void)

{-

\**** REACTION SYSTEMS ****

\$ E.NICOLAS 12100466 ethan.bento-nicolas@edu.univ-paris13.fr
\$ D.PAHALIN

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

-- ********* SYSTEMES DE TESTS *********

-- Système de réactions

alphaSystem :: [Reaction]
alphaSystem =
  [ Reaction ["egf"] ["e", "p"] ["erbb1"],
    Reaction ["egf"] [] ["erk12"],
    Reaction ["erk12"] [] ["p70s6k"]
  ]

-- Séquences de tests

betaSequence :: [Sequence]
betaSequence =
  [ ["egf"],
    ["egf", "e"],
    ["erk12"],
    ["p"]
  ]

-- ******** PROCESSUS D'ENVIRONNEMENT *********

{-
Fonction de processus : fonction qui pars de empty,
ajoute ce qu'elle veut a l'env et produit une sortie,
on reutilise la sortie dans l'environnement t+1 et on
réapplique les regles avec encore une entite aleatoire
fournie par la fonction
-}

data Generateur = String -- Rec.X (a.X + b.X) -> la fonction ajoute a ou b recursivement a la liste des entites presentes a (t - 1)

{- processus :: [Entites] -> Generateur -> [Reaction] -> [Entites]
processus = loop state return
processus e g r = (verifSequence g(e) r)  g  r -}

-- ******** TODO *********

-- Lecture des réactions dans un fichier

-- Langage proposition logique pour exprimer le systeme (egf ∧ ¬e ∧ ¬p -> errb1)