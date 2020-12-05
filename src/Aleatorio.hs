module Aleatorio where

import DecisionTree
import ReadData


import Data.List (transpose)
import System.Random
import System.Random.Shuffle


-- Seleciona os exemplos a serem utilizados para cada árvore
-- Realiza a amostragem de exemplos e de features utilizando de funções auxiliares
sampleExamples ::  RandomGen gen => gen -> [Example] -> Int -> Int -> ([FeatureIdentifier], [Example])
sampleExamples g examples nFeatures nExamples = sampledExamples
    where
        sampledLines = sampleLines gLines nExamples examples
        sampledExamples = sampleFeatures gFeatures nFeatures sampledLines
        (gFeatures, gLines) = split g


-- Realiza a amostragem de features dos exemplos
sampleFeatures :: RandomGen gen => gen -> Int -> [Example] -> ([FeatureIdentifier], [Example])
sampleFeatures g n examples = (selectedNumbers,  zipWith Example selectedFeatures (classe <$> examples))
    where
        nFeatures = length . transpose $ features <$> examples
        selectedNumbers = severalNumbers n [0..nFeatures-1] g
        selectedFeatures = selectFeatures examples selectedNumbers

        -- Realiza a amostragem dos n valores de features a serem utilizados
        severalNumbers :: RandomGen gen => Int -> [a] -> gen -> [a]
        severalNumbers n xs g = take n $ shuffle' xs (length xs) g


-- Realiza a amostragem dos exemplos (bootstrap)
sampleLines :: RandomGen gen => gen -> Int -> [Example] -> [Example]
sampleLines g n examples = takeLines examples selectedNumbers
    where
        selectedNumbers = take n $ randomRs (0, length examples-1) g


-- Dado os números já amostrados, seleciona as features relacionadas
selectFeatures :: [Example] -> [Int] -> [[Feature]]
selectFeatures examples identifiers = transpose $ takeLines transposedFeatures identifiers
    where
        transposedFeatures = transpose $ features <$> examples


-- Dado uma lista qualquer, pega os elementos nas posições fornecidas.
takeLines :: [a] -> [Int] -> [a]
takeLines xs ns = [xs !! n | n <- ns]
