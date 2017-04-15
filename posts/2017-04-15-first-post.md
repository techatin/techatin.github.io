---
title: First Legitimate Post
author: Techatin
bios: New to hakyll, wasted a ton of time trying to do the css, and now finally here
tags: random
---

Is this my first legitimate post? I think so. Let me just test out a few things that I have set up!

<!--more-->

#### Inline Latex

> With a time complexity of $\mathcal{O}(\log{}n)$, binary search is way superior as compared to linear search

> $\sum^{\infty}_{i=1}{i} = -12$

#### Inline dot Graph

```{lang="dot"}
digraph graphName { a -> b; b -> c; a -> c; }
```
#### Inline Code Syntax Highlighting

```{.haskell .numberLines}

-- exercise 3
-- some ground work...

data Stream a = Cons a (Stream a)

streamToList (Cons a b) = a:(streamToList b)

instance Show a => Show (Stream a) where
    show = show. take 20 .streamToList

-- exercise 4
-- more ground work...

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- exercise 5
-- even more ground work...

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) z = Cons x (interleaveStream z xs)

startRuler :: Integer -> Stream Integer
startRuler y = interleaveStream (streamRepeat y) (startRuler (y + 1))

ruler :: Stream Integer
ruler = startRuler 0

-- exercise 6
-- generating function!!! (suan)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger x = Cons x (streamRepeat 0)
    negate = streamMap (0-)
    (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
    (*) (Cons x xs) s@(Cons y ys) = Cons (x * y) ((streamMap (*x) ys) + xs * s)

instance Fractional (Stream Integer) where
    (/) (Cons x xs) (Cons y ys) = q where
        q = Cons (x `div` y) (streamMap (`div` y) (xs - q * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

```
