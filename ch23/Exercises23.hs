module Exercises23 where

import Control.Monad.Trans.State

get :: State s s
get = state $ \s -> (s, s)

put :: s -> State s ()
put s = state $ \x -> ((), s)

exec :: State s a -> s -> s
exec st = snd . runState st

eval :: State s a -> s -> a
eval st = fst . runState st

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)
