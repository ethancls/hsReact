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
type Sequence = [String]

type Entites = String

-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactifs :: [Entites], inhibiteurs :: [Entites], produits :: [Entites]} deriving (Show, Eq)

-- ******** FONCTIONS DE VERIFICATIONS ********

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
verifReac :: Sequence -> Reaction -> Bool
verifReac sequence reaction =
  all (`elem` sequence) (reactifs reaction) && not (any (`elem` sequence) (inhibiteurs reaction))

-- Test des séquences sur l'ensemble des réactions
verifSequence :: [Sequence] -> [Reaction] -> [Entites]
verifSequence sequences reactions =
  concatMap produits (filter (\reaction -> any (`verifReac` reaction) sequences) reactions)

-- Verification de présence d'une entité
verifEntite :: [Entites] -> [Sequence] -> [Reaction] -> Bool
verifEntite entites sequences reactions = any (`elem` verifSequence sequences reactions) entites

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
  [ ["egf", "e"]
  ]

-- ******** PROCESSUS D'ENVIRONNEMENT *********

-- Fonction de processus

-- ******** TODO *********

-- Lecture des réactions dans un fichier

-- Langage proposition logique pour exprimer le systeme (egf ∧ ¬e ∧ ¬p -> errb1)