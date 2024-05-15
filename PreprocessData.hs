module PreprocessData where
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.IO
import Control.Exception (catch, SomeException)
import Data.Char (toLower)

type DataMap = Map.Map String Int

-- Normalize characters in the string
normalizeKey :: String -> String
normalizeKey = filter (/= ' ') . map normalizeChar . map toLower
  where
    normalizeChar '。' = 'o'  -- Convert ideographic full stop to 'o'
    normalizeChar 'O'  = 'o'  -- Convert 'O' to 'o'
    normalizeChar 'o'  = 'o'  -- Ensure 'o' remains 'o'
    normalizeChar '姗' = '娜'
    normalizeChar c    = c

-- Function to read data from a file and convert it to a Map, normalizing keys and removing exact duplicates
readData :: FilePath -> IO DataMap
readData filePath = do
    contents <- readFile filePath
    let linesOfContent = lines contents
    return $ Map.fromList $ mapMaybe parseLine linesOfContent  -- Use fromList to overwrite duplicates
  where
    parseLine :: String -> Maybe (String, Int)
    parseLine line = case reverse $ words line of
        (valueStr : nameParts) -> case reads valueStr of
            [(value, "")] -> Just (normalizeKey $ unwords $ reverse nameParts, value)
            _ -> Nothing
        _ -> Nothing

-- Function to write the preprocessed data back to a file
writeData :: FilePath -> DataMap -> IO ()
writeData filePath dataMap = do
    let linesToWrite = map (\(key, value) -> key ++ "\t" ++ show value) $ Map.toList dataMap
    writeFile filePath (unlines linesToWrite)

-- Main function to load data, preprocess it, and write the results back


main :: IO ()

main = catch runMain handler
  where
    runMain = do
      putStrLn "Loading and normalizing data from week1.txt..."
      week1Data <- readData "week1.txt"
      writeData "week1_normalized.txt" week1Data
      putStrLn "Processed week 1 data saved."

      putStrLn "\nLoading and normalizing data from week2.txt..."
      week2Data <- readData "week2.txt"
      writeData "week2_normalized.txt" week2Data
      putStrLn "Processed week 2 data saved."

    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "An error occurred: " ++ show ex
