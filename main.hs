#!/usr/bin/runhaskell

import System.Environment

main = do
  args <- getArgs
  (mapM (\l -> putStrLn ("Point for " ++ show l ++ " ? : ") >> getLine >>= (\p -> return (read p::Int))) $ head args) >>= return . foldr (+) 0 >>= (\x->do r<-(putStrLn "Bonus coef ? : " >> getLine >>= (\b -> return (read b::Int))); print $ r*x)
