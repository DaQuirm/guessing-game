{-# LANGUAGE DeriveFunctor #-}

module Free where

data Free f a
  = Free (f (Free f a))
  | Pure a
  deriving Functor

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure fa <*> Pure a = Pure $ fa a
  Pure fa <*> Free a = Free $ (fa <$>) <$> a
  Free fa <*> a = Free $ (<*> a) <$> fa

instance (Functor f) => Monad (Free f) where
  Pure a >>= f = f a
  Free a >>= f = Free $ (>>= f) <$> a

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure
