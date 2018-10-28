{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Random (RandomGen, randomR, randomRIO)
import Control.Monad.State.Lazy (MonadState(get, put), evalStateT, modify)
import Control.Monad.IO.Class (MonadIO, liftIO)

type AppState = Int -- number of attempts

printLine :: (MonadIO m) => String -> m ()
printLine = liftIO . putStrLn

readLine :: (MonadIO m) => m String
readLine = liftIO getLine

generate :: (MonadIO m) => Int -> Int -> m Int
generate from to = liftIO $ randomRIO (from, to)

loop :: (MonadIO m, MonadState AppState m) => Int -> m ()
loop secret = do
  attempts <- get
  if attempts == 0 then
    printLine $ "game over! it was " ++ show secret
  else do
    printLine "Make a guess: "
    input <- readLine
    let guess = read input
     in case compare secret guess of
          LT -> printLine "too big!"   >> continue
          GT -> printLine "too small!" >> continue
          EQ -> printLine $ "you won! it's indeed " ++ show secret ++ "!"
          where
            continue = do
              modify $ subtract 1
              loop secret

app :: (MonadIO m, MonadState AppState m) => m ()
app = do
  secret <- generate 1 10
  loop secret

attempts_per_game :: Int
attempts_per_game = 3

main :: IO ()
main = evalStateT app attempts_per_game
