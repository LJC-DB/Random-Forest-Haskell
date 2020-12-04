module Main where

import RandomForest
import DecisionTree
import ReadData


import System.Random


paramsTree :: DTreeParameters
paramsTree = DTreeParameters 200 100 5  -- minSampleSplit, minSampleLeaf, maxDepth

paramsForest :: RFParameters
paramsForest = RFParameters 100 4000 5 paramsTree -- nEstimators, nSamples, nFeatures


main :: IO ()
main = do
    ex <- loadExamples HasHeader "train.csv"
    test <- loadExamples HasHeader "test.csv"
    let dTree' = trainDecisionTree ex paramsTree
    case dTree' of
        Nothing -> putStrLn "An error happened. Tree not formed"
        Just dTree -> do
            let predictionsTrain = predictDecisionTree dTree ex
                accTrain = accuracy (classe <$> ex) predictionsTrain
                predictionsTest = predictDecisionTree dTree test
                accTest = accuracy (classe <$> test) predictionsTest
            putStrLn $ "Tree Acc Train: " ++  show accTrain
            putStrLn $ "Tree Acc Test: " ++  show accTest
    g <- getStdGen
    let rf' = trainRandomForest ex paramsForest g
    case rf' of
        Nothing -> putStrLn "An error happened. Forest not formed"
        Just rf -> do
            let predictionsTrain = predictRandomForest rf ex
                accTrain = accuracy (classe <$> ex) predictionsTrain
                predictionsTest = predictRandomForest rf test
                accTest = accuracy (classe <$> test) predictionsTest
            putStrLn $ "Forest Acc Train: " ++  show accTrain
            putStrLn $ "Forest Acc Test: " ++  show accTest
