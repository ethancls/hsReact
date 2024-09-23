-- Définition d'une réaction avec les réactifs, les inhibiteurs et les produits
data Reaction = Reaction {reactants :: [String], inhibitors :: [String], products :: [String]} deriving (Show, Eq)

-- Vérifier si une réaction est activée (c'est-à-dire tous les réactifs sont présents et aucun inhibiteur n'est présent)
reacVerif :: [String] -> Reaction -> Bool
reacVerif sequence reaction =
  all (`elem` sequence) (reactants reaction) && not (any (`elem` sequence) (inhibitors reaction))

-- Test des séquences sur l'ensemble des réactions
sequenceVerif :: [[String]] -> [Reaction] -> [String]
sequenceVerif sequences reactions =
  concatMap products (filter (\reaction -> any (`reacVerif` reaction) sequences) reactions)

-- Système de réactions

alphaSystem :: [Reaction]
alphaSystem =
  [ Reaction ["egf"] ["e", "p"] ["erbb1"],
    Reaction ["egf"] [] ["erk12"],
    Reaction ["erk12"] [] ["p70s6k"]
  ]

-- Séquences de tests

betaSequence :: [[String]]
betaSequence =
  [
    ["egf"]
  ]



-- Processus 

processK :: [String] ->  -> [String]
processK


-- TODO 

-- Lecture des réactions dans un fichier

-- 