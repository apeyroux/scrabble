{-
lettre

0 point : Joker ×2 (appelés en français jokers ou lettres blanches)
1 point : E ×15, A ×9, I ×8, N ×6, O ×6, R ×6, S ×6, T ×6, U ×6, L ×5
2 points : D ×3, M ×3, G ×2
3 points : B ×2, C ×2, P ×2
4 points : F ×2, H ×2, V ×2
8 points : J ×1, Q ×1
10 points : K ×1, W ×1, X ×1, Y ×1, Z ×1

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
  point l = (*) (fst $ head $ Map.toList $ Map.filter (== True) $ Map.mapWithKey (\_ v -> elem (lSigne l) v) initData) (bonnus2int (lBonnus l))

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
  m <- getLine
  putStr $ "Word bonus's word (" ++ m ++ ") : "
  bm <- getLine
  putStrLn $ "Point : " 
  ls <- (mapM (\l-> (putStr $ "Bonnus " ++ show l ++ " ? ") >> (getLine) >>= (\c-> return $  Letter l (char2bonnus (head c)))) m)
  let wds = Word ls (char2bonnus (head bm))
  putStrLn $ "Resultat pour " ++ m ++ " " ++ (show $ point wds)
