module ReadData where

import Data.List.Split

data Example = Example {features :: [Feature], classe :: ResultClass} deriving (Show)
type Feature = String
type ResultClass = String
data Header = NoHeader | HasHeader

createExample :: String -> Example
createExample linha =
    Example (init columns) (last columns)
    where columns = splitOn "," linha

loadExamples :: Header -> FilePath -> IO [Example]
loadExamples NoHeader fp = do
    l <- lines <$> readFile fp
    return $ createExample <$> l
loadExamples HasHeader fp = do
    l <- lines <$> readFile fp
    return $ createExample <$> tail l
