module StateMonad (
  State,

  return,
  (>>=),

  evalState,
--   execState,
--   getState,
--   updState,
  runState
) where

newtype State s a = State {runState :: s -> (a, s)}

-- Ejercicio 3: Implementar la monada State.

instance Monad (State s) where
   --return :: a -> State s a
   return x = State $ \s -> (x, s)

   --(>>=) :: State s a -> (a -> State s b) -> State s b
--    (>>=) (State fs) f = f.fst.fs


evalState :: State s a -> s -> a
evalState m = fst.(runState m)
--
-- execState :: State s a -> s -> s
-- -- COMPLETAR
--
-- getState :: State s s
-- -- COMPLETAR
--
-- updState :: (s -> s) -> State s ()
-- -- COMPLETAR
--
