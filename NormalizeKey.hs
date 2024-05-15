module NormalizeKey where

import Data.Char (toLower)

-- Normalize characters in the string
normalizeKey :: String -> String
normalizeKey = filter (/= ' ') . map normalizeChar . map toLower
  where
    normalizeChar '。' = 'o'  -- Convert ideographic full stop to 'o'
    normalizeChar 'O'  = 'o'  -- Convert 'O' to 'o'
    normalizeChar 'o'  = 'o'  -- Ensure 'o' remains 'o'
    normalizeChar '姗' = '娜'
    normalizeChar c    = c
