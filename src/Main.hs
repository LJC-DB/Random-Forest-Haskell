module Main where

import DecisionTree
import ReadData
import Aleatorio


import Data.List
import System.Random


data RFParameters = RFParameters {nEstimators :: Int, nSamples :: Int, nFeatures :: Int, treeParameters :: DTreeParameters}
data RandomForest = RF {forestExamples :: [Example], estimators :: [([FeatureIdentifier], DTree)]}

instance Show RandomForest where
    show (RF e trees) = "Random Forest: exemp " ++ (show $ length e) ++ ", estimators\n\n" ++ (intercalate "\n" $ show . snd <$> trees)


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




trainRandomForest :: RandomGen gen => [Example] -> RFParameters -> gen -> Maybe RandomForest
trainRandomForest examples parameters g =
    case maybetrees of
        Nothing -> Nothing
        Just trees -> Just $ RF examples $ zip identifiers trees
    where
        generated = generateNTrees (nEstimators parameters) g examples parameters
        identifiers = fmap fst generated
        maybetrees = sequenceA $ fmap snd generated


generateNTrees :: RandomGen gen => Int -> gen -> [Example] -> RFParameters -> [([FeatureIdentifier], Maybe DTree)]
generateNTrees 0 _ _ _ = []
generateNTrees n g examples parameters =
    let tree = trainDecisionTree sampledExamples (treeParameters parameters) in
        (sampledFeatures, tree) : generateNTrees (n-1) gNext examples parameters
    where
        (sampledFeatures,sampledExamples) = sampleExamples gActual examples (nFeatures parameters) (nSamples parameters)

        (gActual, gNext) = split g


predictRandomForest :: RandomForest -> [Example] -> [ResultClass]
predictRandomForest rf examples = majorityVote $ zipWith predictDecisionTree trees treesExamples
    where
        trees = snd <$> estimators rf
        identifiers = fst <$> estimators rf
        selectedFeatures = (selectFeatures examples) <$> identifiers  -- Filtra as features selecionadas para cada árvore durante treino
        selectExamples [] _ = []
        selectExamples (f:fs) c = zipWith Example f c : selectExamples fs c  -- Associa as features de cada árvore com as classes (que são comuns a todas)
        treesExamples = selectExamples selectedFeatures (classe <$> examples)



-- Recebe as predições geradas por cada árvore da floresta e seleciona a predição mais frequente
majorityVote :: [[ResultClass]] -> [ResultClass]
majorityVote lss = snd . maximum . frequency <$> transpose lss
