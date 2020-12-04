module ReadData where

import Data.List.Split
import Text.Read


data Example = Example {features :: [Feature], classe :: ResultClass} deriving (Show)
type Feature = Either String Double
type ResultClass = String
data Header = NoHeader | HasHeader

createExample :: String -> Example
createExample linha =
    Example (readNum <$> init columns) (last columns)
    where
        columns = splitOn "," linha
        readNum f = let value = readMaybe f :: Maybe Double in
            case value of
                Nothing -> Left f
                Just n  -> Right n

loadExamples :: Header -> FilePath -> IO [Example]
loadExamples NoHeader fp = do
    l <- lines <$> readFile fp
    return $ createExample <$> l
loadExamples HasHeader fp = do
    l <- lines <$> readFile fp
    return $ createExample <$> tail l
