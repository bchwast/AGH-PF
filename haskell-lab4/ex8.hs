module Stack
  ( Stack
  , empty   -- :: Stack a
  , isEmpty -- :: Stack a -> Bool
  , push    -- :: a -> Stack a -> Stack a
  , top     -- :: Stack a -> a
  , pop     -- :: Stack a -> (a,Stack a)
  ) where

-- interface (signature, contract)
empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)

-- implementation
newtype Stack a = MkStack [a] deriving Show -- hidden constructor (see the module export list)

empty = MkStack []
isEmpty (MkStack s) = null s
push x (MkStack s) = MkStack (x:s)
top (MkStack s) = head s
pop (MkStack (s:ss)) = (s,MkStack ss)

module Queue
  ( Queue
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  ) where

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)

newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue s) = null s
addQ x (MkQueue s) = MkQueue (s ++ [x])
remQ (MkQueue (s:ss)) = (s, MkQueue ss)

module Dequeue
    ( Dequeue
    , emptyDEQ
    , isEmptyDEQ
    , lengthDEQ
    , firstDEQ
    , lastDEQ
    , takeFrontDEQ
    , takeBackDEQ
    , pushFrontDEQ
    , popFrontDEQ
    , pushBackDEQ
    , popBackDEQ
    , fromListDEQ
    ) where

emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int -- O(1)
firstDEQ :: Dequeue a -> Maybe a --  O(1)
lastDEQ :: Dequeue a -> Maybe a -- O(1)
takeFrontDEQ :: Int -> Dequeue a -> [a] -- O(n)
takeBackDEQ :: Int -> Dequeue a -> [a] -- O(n)
pushFrontDEQ :: Dequeue a -> a -> Dequeue a -- O(1) amortised
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a) -- O(1) amortised
pushBackDEQ :: Dequeue a -> a -> Dequeue a -- O(1) amortised
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a) -- O(1) amortised
fromListDEQ :: [a] -> Dequeue a -- O(n)

newtype Dequeue a = MkDequeue [a] deriving Show

emptyDEQ = MkDequeue []

isEmptyDEQ (MkDequeue xs) = null xs

lengthDEQ (MkDequeue xs) = length xs

firstDEQ (MkDequeue []) = Nothing
firstDEQ (MkDequeue (x:xs)) = Just x

lastDEQ (MkDequeue []) = Nothing
lastDEQ (MkDequeue xs) = Just (last xs)

takeFrontDEQ n (MkDequeue (x:xs)) | n == 0 = []
                                  | otherwise = x : takeFrontDEQ (n-1) (MkDequeue xs)
takeBackDEQ n (MkDequeue xs) | n == 0 = []
                             | otherwise = takeBackDEQ (n-1) (MkDequeue (init xs)) ++ [last xs]

pushFrontDEQ (MkDequeue xs) x = MkDequeue (x:xs)

popFrontDEQ (MkDequeue [])     = Nothing
popFrontDEQ (MkDequeue (x:xs)) = Just (x, MkDequeue xs)

pushBackDEQ (MkDequeue xs) x = MkDequeue (xs ++ [x])

popBackDEQ (MkDequeue []) = Nothing
popBackDEQ (MkDequeue xs) = Just (last xs, MkDequeue (init xs))

fromListDEQ = MkDequeue