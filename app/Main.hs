{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Random (RandomGen, randomR, randomRIO)

import Lib

data App
  = PrintLine String App
  | ReadLine (String -> App)
  | Generate Int Int (Int -> App)
  | End

runApp :: App -> IO ()
runApp End                        = pure ()
runApp (PrintLine string next)    = do
  putStrLn string
  runApp next
runApp (ReadLine handler)         = do
  input <- getLine
  runApp $ handler input
runApp (Generate from to handler) = do
  secret <- randomRIO (from, to) :: IO Int
  runApp $ handler secret

loop :: Int -> Int -> App
loop secret 0        = PrintLine ("game over! it was " ++ show secret) End
loop secret attempts =
  PrintLine "Make a guess: " $
    ReadLine $ \input ->
      let guess = read @Int input
       in case compare secret guess of
            LT -> PrintLine "too big!" continue
            GT -> PrintLine "too small!" continue
            EQ -> PrintLine ("you won! it's indeed " ++ show secret ++ "!") End
            where continue = loop secret $ attempts - 1

app :: App
app = Generate 1 10 $ \secret -> loop secret 3

main :: IO ()
main = runApp app
