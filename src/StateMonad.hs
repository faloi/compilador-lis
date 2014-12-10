module StateMonad (
  State,
  state,

  return,
  (>>=),

  evalState,
  execState,
  getState,
  updState,
  runState,
  errState
) where

import MayFail

newtype State s a = State {runState :: s -> MayFail (a, s)}

instance Monad (State s) where
   return x = State $ \s -> Ok (x, s)

   (>>=) m f = State $ \s ->
      let result = runState m s
      in case result of
       Ok (a, s') -> runState (f a) s'
       Raise ex -> Raise ex

-- No se por que pero no puedo usar el constructor desde el test
state :: (s -> MayFail (a, s)) -> State s a
state = State

evalState :: State s a -> s -> MayFail a
evalState m s = let result = runState m s
              in case result of
               Ok (a, _) -> Ok a
               Raise ex -> Raise ex

execState :: State s a -> s -> MayFail s
execState m s = let result = runState m s
              in case result of
               Ok (_, s) -> Ok s
               Raise ex -> Raise ex

getState :: State s s
getState = State $ \x -> Ok (x, x)

updState :: (s -> s) -> State s ()
updState f = State $ \s -> Ok ((), f s)

errState :: Exception -> State s a
errState ex = State $ \_ -> Raise ex
