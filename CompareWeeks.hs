module CompareWeeks where
import NormalizeKey (normalizeKey)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)  -- Ensure fromMaybe is included
import System.IO
import Control.Exception (catch, SomeException)
import Data.Char (toLower)

type DataMap = Map.Map String Int

-- Function to read data from a file and convert it to a Map, normalizing keys
readData :: FilePath -> IO DataMap
readData filePath = do
    contents <- readFile filePath
    let linesOfContent = lines contents
    return $ Map.fromListWith (+) $ mapMaybe parseLine linesOfContent
  where
    parseLine :: String -> Maybe (String, Int)
    parseLine line = case words line of
      [key, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (normalizeKey key, value)
        _ -> Nothing
      _ -> Nothing

-- Function to compute differences between two DataMaps: week2 - week1
computeDifferences :: DataMap -> DataMap -> DataMap
computeDifferences newData oldData = 
    Map.unionWith (-) (Map.mapWithKey (\key new -> new - fromMaybe 0 (Map.lookup key oldData)) newData)
                       (Map.mapWithKey (\key old -> 0 - old) (oldData `Map.difference` newData))

-- Main function to load data, compute differences, and print the results

main :: IO ()

main = catch runMain handler
  where
    runMain = do
      putStrLn "Loading data from files..."
      oldData <- readData "week1_normalized.txt"
      newData <- readData "week2_normalized.txt"
      let differences = computeDifferences newData oldData
      putStrLn "Differences between the new week and the previous week:"
      mapM_ printDifference $ Map.toList differences
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "An error occurred: " ++ show ex

printDifference :: (String, Int) -> IO ()
printDifference (member, diff) 
  | diff /= 0 = putStrLn $ member ++ ": " ++ show diff
  | otherwise = return ()
