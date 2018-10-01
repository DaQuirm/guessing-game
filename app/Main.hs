{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import System.Random (RandomGen, randomR, randomRIO)

data Free f a
  = Free (f (Free f a))
  | Pure a
  deriving (Functor)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure fa <*> Pure a = Pure $ fa a
  Pure fa <*> Free a = Free $ (fa <$>) <$> a
  Free fa <*> a = Free $ (<*> a) <$> fa

instance (Functor f) => Monad (Free f) where
  Pure a >>= f = f a
  Free a >>= f = Free $ (>>= f) <$> a

data AppF a
  = PrintLine String a
  | ReadLine (String -> a)
  | Generate Int Int (Int -> a)
  | End

type App = Free AppF

-- runApp :: App -> IO ()
-- runApp End                        = pure ()
-- runApp (PrintLine string next)    = do
--   putStrLn string
--   runApp next
-- runApp (ReadLine handler)         = do
--   input <- getLine
--   runApp $ handler input
-- runApp (Generate from to handler) = do
--   secret <- randomRIO (from, to) :: IO Int
--   runApp $ handler secret
--
-- loop :: Int -> Int -> App
-- loop secret 0        = PrintLine ("game over! it was " ++ show secret) End
-- loop secret attempts =
--   PrintLine "Make a guess: " $
--     ReadLine $ \input ->
--       let guess = read @Int input
--        in case compare secret guess of
--             LT -> PrintLine "too big!" continue
--             GT -> PrintLine "too small!" continue
--             EQ -> PrintLine ("you won! it's indeed " ++ show secret ++ "!") End
--             where continue = loop secret $ attempts - 1
--
-- app :: App
-- app = Generate 1 10 $ \secret -> loop secret 3
--
-- main :: IO ()
-- main = runApp app

main = pure ()
