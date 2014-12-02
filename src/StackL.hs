module StackL(
    Stack,

    empty,
    isEmpty,
    push,
    top,
    pop
) where

-- Ejercicio 1: Implementar el tipo abstracto Stack.

data Stack a = Stack [a]

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

top :: Stack a -> a
top (Stack (x:xs)) = x

pop :: Stack a -> Stack a
pop (Stack (x:xs)) = Stack xs
