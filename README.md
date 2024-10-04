# 🧪 Système de Réaction 🧪

## 🚀 Projet Système de Réaction

Bienvenue dans le mini-projet **Système de Réaction** développé par **Ethan Nicolas** et **Dmytro Palahin** basé sur la condition de notre professeur **Carlos Olarte**. Ce projet vise à modéliser et analyser des systèmes de réaction chimique en utilisant des séquences d'entités et des réactions définies.

## 🎯 Table des Matières

1. Introduction
2. Fonctionnalités
3. Installation
4. Utilisation

## 🎬 Introduction

Ce projet permet de modéliser des systèmes de réaction chimique et de vérifier diverses propriétés logiques sur les séquences d'entités générées par ces systèmes. Les principales fonctionnalités incluent l'observation des sorties d'un système de réaction (RS), l'interaction avec un processus K, et la vérification de la production d'entités spécifiques.

## ✨ Fonctionnalités

- **🔍 Observation des sorties d'un RS** sous une séquence d'entrée donnée.
- **🔁 Interaction avec un processus K** pour observer le comportement d'un RS.
- **⚛️ Vérification de la production d'entités** spécifiques dans un RS lors de l'interaction avec un processus K.
- **✅ Manipulation de formules logiques** pour vérifier des propriétés complexes sur les séquences d'entités.

## 🚧 Installation

Pour installer et exécuter ce projet, suivez les étapes ci-dessous :

1. Clonez le dépôt :

    ```bash
    git clone https://github.com/ethancls/ReactionsSystems.git
    cd ReactionsSystems
    ```

2. Assurez-vous d'avoir GHC (Glasgow Haskell Compiler) installé et GHCI. Vous pouvez l'installer via [Haskell Platform](https://www.haskell.org/platform/).

3. Compilez le projet :

    ```bash
    ghci
    :l src/hsreact.hs
    ```

4. Exécutez le programme :

    ```bash
    hsreact
    ```

## 📋 Utilisation

### 1. Observer la sortie d'un RS sous une séquence d'entrée donnée

Pour observer la sortie d'un système de réaction sous une séquence d'entrée donnée, utilisez la fonction `reacChaine`. Par exemple :

```haskell
reacChaine ["egf"] alphaSystem
```

où **alphaSystem** est

```haskell
alphaSystem :: [Reaction]
alphaSystem =
    [ Reaction ["egf"] ["e", "p"] ["erbb1"]
    , Reaction ["egf"] [] ["erk12"]
    , Reaction ["erk12"] [] ["p70s6k"]
    , Reaction ["a"] ["b"] ["c"]
    , Reaction ["c"] ["a"] ["d"]
    ]
```

et on obtient ce résultat :

```haskell
[["erbb1","erk12"],["p70s6k"]]
```

### 2. Observer le comportement d'un RS lors de l'interaction avec un processus K

Pour observer le comportement d'un RS lors de l'interaction avec un processus K, utilisez la fonction `recK`. Par exemple :

```haskell
recK [["egf"]] alphaSystem
```

### 3. Vérifiez si une entité donnée est produite dans un RS lors de l'interaction avec un processus K

Pour vérifier si une entité donnée est produite dans un RS lors de l'interaction avec un processus K, utilisez la fonction `recK`. Par exemple :

```haskell
recK [["a", "b", "c"], ["c", "a", "d"]] alphaSystem
```

### 4. Vérifiez si le RS se stabilise

Pour vérifier si le système de réaction se stabilise, c'est-à-dire s'il existe un cycle où un état est visité une infinité de fois, on peut toujours utiliser la fonction `recK`. Et à la fin, on peut voir à quelle étape notre système se stabilise.

### 5. Résultats avec d'autres données

Pour utiliser les données fournies par notre professeur **Carlos Olarte**, il suffit d'utiliser la fonction principale :

```haskell
hsreact
```

Vous aurez alors la possibilité de choisir parmi des ensembles de données plus volumineux.

Tests effectués par rapport à la **Table 1 [page 5 de paper.pdf]**, stimulis **egf**, **hrg** et drogues **e**, **p**, **t**. Nous avons testé toutes les combinaisons de stimulis et de drogues (**empty** également) et avons obtenu les mêmes résultats. Dans **generateur.txt**, on entre la combinaison par exemple **"egf,hrg,e,t"** et on teste sur les réactions **short-term**.
Pour les **long-term**, on doit rajouter le composé **"s"** aux stimulis.
