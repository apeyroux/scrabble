{-
haskell school
scrabble calculator
-}

import qualified Data.Map as Map
import Data.Monoid

data Bonnus = Double | Triple deriving (Show, Eq)

data Letter = Letter {
  lSigne :: Char,
  lBonnus :: Maybe Bonnus
} deriving Show

data Word = Word {
  wSignes :: [Letter],
  wBonnus :: Maybe Bonnus
} deriving Show

class Scrabble l where
  point :: l -> Integer

instance Scrabble Letter where
  point l = (*) (fst $ head $ Map.toList $ Map.filter (== True) $
                 Map.mapWithKey (\_ v -> elem (lSigne l) v) initData) (bonnus2int (lBonnus l))

instance Scrabble Word where
  point w = (*) (foldr (+) 0 $ map point $ wSignes w) (bonnus2int (wBonnus w))

bonnus2int :: Maybe Bonnus -> Integer
bonnus2int b | b == (Just Double) = 2
             | b == (Just Triple) = 3
             | b == Nothing = 1

char2bonnus :: Char -> Maybe Bonnus
char2bonnus b | b == 'd' = Just Double
              | b == 't' = Just Triple
              | otherwise = Nothing

initData :: Map.Map Integer [Char]
initData = Map.singleton 1 ['e','a','i','n','o','r','s','t','u','l'] <>
           Map.singleton 2 ['d','m','g'] <>
           Map.singleton 3 ['b','c','p'] <>
           Map.singleton 4 ['f','h','v'] <>
           Map.singleton 8 ['j','q'] <>
           Map.singleton 10 ['k','w','x','y','z']

main :: IO()
main = do
  putStr "Word : "
  w <- getLine
  putStr $ "Word bonus's for " ++ w ++ " (d/t/n) : "
  bonnusw <- getLine
  letters <- (mapM (\l-> (putStr $ "Bonnus " ++ show l ++ " (d/t/n) ? ") >>
                         (getLine) >>=
                         (\c-> return $  Letter l (char2bonnus (head c)))) w)
  putStrLn $ "Result for " ++ w
    ++ " " ++ (show $ point $ Word letters (char2bonnus (head bonnusw)))
