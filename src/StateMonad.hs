module StateMonad (
  State,
  state,

  return,
  (>>=),

  evalState,
  execState,
  getState,
  updState,
  runState
) where

newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
   return x = State $ \s -> (x, s)

   (>>=) m f = State $ \s ->
      let (a, s') = runState m s
      in runState (f a) s'

-- No se por que pero no puedo usar el constructor desde el test
state :: (s -> (a, s)) -> State s a
state = State

evalState :: State s a -> s -> a
evalState m = fst.(runState m)

execState :: State s a -> s -> s
execState m = snd.(runState m)

getState :: State s s
getState = State $ \x -> (x, x)

updState :: (s -> s) -> State s ()
updState f = State $ \s -> ((), f s)
