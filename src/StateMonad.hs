module StateMonad (
  State,

  return,
  (>>=),

  evalState,
  execState,
  getState,
--   updState,
  runState
) where

newtype State s a = State {runState :: s -> (a, s)}

-- Ejercicio 3: Implementar la monada State.

instance Monad (State s) where
   --return :: a -> State s a
   return x = State $ \s -> (x, s)

   --(>>=) :: State s a -> (a -> State s b) -> State s b
   (>>=) m f = State $ \s ->
      let (a, s') = runState m s
      in runState (f a) s'

evalState :: State s a -> s -> a
evalState m = fst.(runState m)

execState :: State s a -> s -> s
execState m = snd.(runState m)

getState :: State s s
getState = State $ \x -> (x, x)
--
-- updState :: (s -> s) -> State s ()
-- -- COMPLETAR
--
