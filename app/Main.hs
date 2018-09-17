{-# LANGUAGE TypeApplications #-}

module Main where

import System.Random (randomRIO)

import Lib

main :: IO ()
main = do
  secret <- randomRIO (1, 10) :: IO Int
  logic secret 3
    where logic secret 0        = putStrLn $ "game over! it was " ++ show secret
          logic secret attempts = do
            guess <- read @Int <$> getLine
            case compare secret guess of
              LT -> putStrLn "too big!"   >> continue
              GT -> putStrLn "too small!" >> continue
              EQ -> putStrLn $ "you won! it's indeed " ++ show secret ++ "!"
              where continue = logic secret $ attempts - 1
