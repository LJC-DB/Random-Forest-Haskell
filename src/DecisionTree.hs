module DecisionTree where

import ReadData


import Data.Map ( fromListWith, toList )
import Data.List ( minimumBy, transpose, nub )
import Data.Tuple
import Data.Ord
import Data.Either


type Depth = Int
type FeatureIdentifier = Int
type Decision a = (FeatureIdentifier, a)


type FeatureWithClass = (Feature, ResultClass)  -- Dado um exemplo, é a associação do valor de uma de suas features e de sua classe
type CountClass = (Double, ResultClass)  -- Salva a quantidade de vezes que a classe aparece


data DTreeParameters = DTreeParameters {minSampleSplit :: Int, minSampleLeaf :: Int, maxDepth :: Int}
data DTree = Leaf {treeExamples :: [Example], label :: ResultClass } |
    Node {treeExamples :: [Example], depth :: Depth, decision :: Decision Feature, sons :: [DTree] }

instance Show DTree where
    show (Leaf e l) = "Leaf: label " ++ l ++ ", exemp " ++ (show $ length e) ++ "\n"
    show (Node e depth d s) = "Node"++show depth++": decision " ++ show d ++
        ", exemp " ++ (show $ length e) ++ ", sons " ++ "\n" ++ (mconcat $ (tabulation ++) . show <$> s)
        where
            tabulation = mconcat $ replicate depth "  "


trainDecisionTree :: [Example] -> DTreeParameters -> Maybe DTree
trainDecisionTree [] _ = Nothing
trainDecisionTree examples parameters = do makeNode examples 1 parameters


predictDecisionTree :: DTree -> [Example] -> [ResultClass]
predictDecisionTree _ [] = []
predictDecisionTree dTree (e:ex) = (label $ walk dTree e) : predictDecisionTree dTree ex


accuracy :: [ResultClass] -> [ResultClass] -> Double
accuracy trueLabels predictedLabels = nTrues / nResults
    where
        nTrues = fromIntegral . length . filter (==True) $ zipWith (==) trueLabels predictedLabels
        nResults = fromIntegral . length $ trueLabels


walk :: DTree -> Example -> DTree
walk (Node _ _ d [trueTree, falseTree]) e =
    if compareWithDecision d e
        then walk trueTree e
        else walk falseTree e
walk leaf _ = leaf


makeLeaf :: [Example] -> DTreeParameters -> Maybe DTree
makeLeaf examples parameters =
    if length examples < minSampleLeaf parameters
        then Nothing
        else Just $ Leaf examples $ maxClass examples


makeNode :: [Example] -> Int -> DTreeParameters -> Maybe DTree
makeNode examples depth parameters
    | depth >= maxDepth parameters = makeLeaf examples parameters
    | length examples < minSampleSplit parameters = makeLeaf examples parameters
    | otherwise =
        case bestFeature examples parameters of
            Nothing -> makeLeaf examples parameters
            Just dec -> do
                let trueExemples  = filter (compareWithDecision dec) examples
                    falseExemples = filter (not . compareWithDecision dec) examples
                    listExamples = [trueExemples, falseExemples]
                generatedSons <- sequenceA $ fmap makeNode listExamples <*> pure (depth+1) <*> pure parameters
                return $ Node examples depth dec generatedSons


-- Dado os exemplos em um nó, decide a melhor feature para separá-los
bestFeature :: [Example] -> DTreeParameters -> Maybe (Decision Feature)
bestFeature examples parameters = if bestGini <= 1 then Just (pos, bestFeat) else Nothing
    where
        bestForFeature = bestQuestion <$> allfeatures examples <*> pure parameters
        ((bestGini, bestFeat), pos) = minimumBy (comparing fst) $ zip bestForFeature [0..]


-- Dado uma determinada coluna de feature, determina o valor que melhor separa as classes e o gini da árvore gerada
bestQuestion :: [FeatureWithClass] -> DTreeParameters -> (Double, Feature) -- Double=Gini
bestQuestion fc parameters=
    let separated = separateTrueFalse fc <$> possibleValues
        ginis = calcGini <$> separated
    in
        minimum $ zip ginis possibleValues
    where
        min = minSampleLeaf parameters
        calcGini (t,f) = if length t >= min && length f >= min
                            then (giniTree . possibleNode) (t,f)
                            else 2  -- Invalid Value


        -- para uma coluna de features, pega os valores que pode assumir
        possibleValues :: [Feature]
        possibleValues = nub [x | (x, _ ) <- fc]


-- Cria nós temporários para permitir calcular impureza
possibleNode :: ([FeatureWithClass], [FeatureWithClass]) -> DTree
possibleNode (t,f) = Node (allExamples) 2 undefined [Leaf trueExamples undefined , Leaf falseExamples undefined]
    where
        trueExamples = fmap (\(f,l) -> Example [f] l) t
        falseExamples = fmap (\(f,l) -> Example [f] l) f
        allExamples = trueExamples ++ falseExamples


-- Recebe uma lista de exemplos e separa em colunas de features
-- Cada coluna é uma lista com o valor da feature com a classe para cada exemplo
allfeatures :: [Example] -> [[FeatureWithClass]]
allfeatures ex = transpose [pure (,) <*> (features x) <*> [classe x] | x <- ex]


--  Recebe uma lista contendo pares de valor da feature com valor da classe para cada exemplo
--  Separa essa lista em duas para um determinado valor da feature
separateTrueFalse :: [FeatureWithClass] -> Feature -> ([FeatureWithClass], [FeatureWithClass])
separateTrueFalse x f =
    if isLeft f then
        ( filter ((== f) . fst) x ,    -- possible True answer
          filter ((/= f) . fst) x )    -- all other classes
    else
        ( filter ((<= f) . fst) x ,    -- possible True answer
          filter (( > f) . fst) x )    -- all other classes



-- Função que recebe a decisão salva em um nó e compara com o valor da feature em um dado exemplo
compareWithDecision :: Decision Feature -> Example -> Bool
compareWithDecision d e =
    if isLeft value then
        features e !! fst d == snd d
    else features e !! fst d <= snd d
    where value = snd d


-- Calcula o indice gini para qualquer ponto da árvore fornecido
giniTree :: DTree -> Double
giniTree (Leaf examples _) = sum $ zipWith (*) probs $ fmap (1-) probs  -- Gini definition for leaf
    where
        frequencies = frequency $ fmap classe $ examples
        probs = fmap (\(f, _) -> f/len) frequencies
        len = fromIntegral $ length examples
giniTree (Node examples _ _ a) =  sum $ zipWith (*) (fmap (/len) nE)  ginis -- Gini definition for node
    where
        nE = fmap nExamples a
        ginis = fmap giniTree a
        len = fromIntegral $ length examples
        nExamples t = fromIntegral $ length $ treeExamples t


-- Recebe uma lista com a classe de cada exemplo calcula a frequencia de cada uma
frequency :: [ResultClass] -> [CountClass]
frequency xs =  fmap swap $ toList (fromListWith (+) [(x, 1) | x <- xs])


-- Retorna a classe mais frequente dada uma lista de exemplos
maxClass :: [Example] -> ResultClass
maxClass examples = snd $ maximum frequencies
    where
        frequencies = frequency $ fmap classe examples
