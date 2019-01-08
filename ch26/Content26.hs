{-# LANGUAGE InstanceSigs #-}

module Content26 where

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = undefined

instance (Monad m) => Applicative (StateT s m) where
    pure = undefined
    (<*>) = undefined

instance (Monad m) => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= f =
        StateT $ \s -> do
           (a, s) <- sma s 
            runStateT (f a) s
