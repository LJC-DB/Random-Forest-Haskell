module Aleatorio where

import DecisionTree
import ReadData


import Data.List (transpose)
import System.Random
import System.Random.Shuffle




severalNumbers :: RandomGen gen => Int -> [a] -> gen -> [a]
severalNumbers n xs g = take n $ shuffle' xs (length xs) g

sampleExamples ::  RandomGen gen => gen -> [Example] -> Int -> Int -> ([FeatureIdentifier], [Example])
sampleExamples g examples nFeatures nExamples = sampledFeatures
    where
        sampledLines = sampleLines gLines nExamples examples
        sampledFeatures = sampleFeatures gFeatures nFeatures sampledLines
        (gFeatures, gLines) = split g


sampleFeatures :: RandomGen gen => gen -> Int -> [Example] -> ([FeatureIdentifier], [Example])
sampleFeatures g n examples = (selectedNumbers,  zipWith Example selectedFeatures (classe <$> examples))
    where
        nFeatures = length . transpose $ features <$> examples
        selectedNumbers = severalNumbers n [0..nFeatures-1] g
        selectedFeatures = selectFeatures examples selectedNumbers


selectFeatures :: [Example] -> [Int] -> [[Feature]]
selectFeatures examples identifiers = transpose $ takeLines transposedFeatures identifiers
    where
        transposedFeatures = transpose $ features <$> examples


sampleLines :: RandomGen gen => gen -> Int -> [Example] -> [Example]
sampleLines g n examples = takeLines examples selectedNumbers
    where
        selectedNumbers = take n $ randomRs (0, length examples-1) g


takeLines :: [a] -> [Int] -> [a]
takeLines _ [] = []
takeLines xs (n:ns) = [xs !! n] ++ takeLines xs ns
