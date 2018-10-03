{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import System.Random (RandomGen, randomR, randomRIO)

import Free(Free(..), liftF)

data AppF a
  = PrintLine String a
  | ReadLine (String -> a)
  | Generate Int Int (Int -> a)
  deriving Functor

type App = Free AppF

runApp :: App a -> IO ()
runApp (Pure a)    = pure ()
runApp (Free (PrintLine string next)) = do
  putStrLn string
  runApp next
runApp (Free (ReadLine handler)) = do
  input <- getLine
  runApp $ handler input
runApp (Free (Generate from to handler)) = do
  secret <- randomRIO (from, to) :: IO Int
  runApp $ handler secret

printLine :: String -> App ()
printLine string = liftF $ PrintLine string ()

readLine :: App String
readLine = liftF $ ReadLine id

generate :: Int -> Int -> App Int
generate from to = liftF $ Generate from to id

loop :: Int -> Int -> App ()
loop secret 0        = printLine $ "game over! it was " ++ show secret
loop secret attempts = do
  printLine "Make a guess: "
  input <- readLine
  let guess = read @Int input
   in case compare secret guess of
        LT -> printLine "too big!"   >> continue
        GT -> printLine "too small!" >> continue
        EQ -> printLine $ "you won! it's indeed " ++ show secret ++ "!"
        where continue = loop secret $ attempts - 1

app :: App ()
app = do
  secret <- generate 1 10
  loop secret 3

main :: IO ()
main = runApp app
