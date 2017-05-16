---
title: A Gentle Introduction to Binomial Heap (Part 1)
author: Techatin
tags: data structures
bios: the thing that is more efficient than a normal heap, and significantly more friendly than fibonacci heap
---

Sometimes in life, one will start questioning: do perfect things ever exist? Well of course they do. Communism, Marymount JC, Haskell, CPM the Protector of Alma Mater - there are just so many of them. Binomial heap - in my opinion is another one of those perfect things.

<!--more-->

So what is a binomial heap? I have a heap:

![](/images/240px-Min-heap.png)

I have binomial:

![](/images/binomial.png)

UHH! BINOMIAL HEAP!

![](/images/binomial_heap.png)

Ok jokes aside. A more useful question to ask might be why on earth do we need binomial heaps in the first place. A regular heap allows us to perform insertion and deletion of minimum element in $\mathcal{O}(\log{}n)$, and get the minimum element in constant time. However, such a heap would fare badly when we want to merge two heaps together, taking a total of $\mathcal{O}(n)$ time. A binomial heap, on the other hand, allows us to even merge two heaps in $\mathcal{O}(\log{}n)$ time. But to grasp the concept of a binomial heap properly, we need to first grasp the concept of a special heap called a binomial tree.

This is a binomial tree of order 0:

```{lang="dot"}
digraph G {
    node [shape="circle"];
    a[label="1"];
}
```

There is nothing too special about it. It is just one node with a value associated. This is a binomial tree of order 1:

```{lang="dot"}
digraph G {
    node [shape="circle"];
    a[label="1"]; b[label="2"];
    a->b;
}
```

There is nothing too special about it either. It is just two nodes with the smaller node pointing to the larger one. But look, this is a binomial tree of order 2:

```{lang="dot"}
digraph G {
    node [shape="circle"];
    a[label="1"]; b[label="2"];
    c[label="3"]; d[label="4"];

    a -> b; a -> c; c -> d;
}
```

It is just two binomial trees of order 1 merged together! Specifically, A binomial tree of order $n + 1$ is just two binomial tree of order $n$ with the roots joined together, rooted at the root node with the smaller value.

```{lang="dot"}
digraph G {
    node [shape="circle"];
    a[shape="triangle"]; b[shape="triangle"]; c[shape="triangle"]; d[shape="triangle"];
    e[label="22"]; f[label="13"];

    f -> e; e -> a; e -> b;
    f -> c; f -> d;
}
```

Now it is the time for a surprise: a binomial heap is not a heap!!!! It is in fact a forest of binomial trees, like the one presented above, such that for any natural number n, there is either one or no binomial tree of order n. Now that we have the definition of a binomial heap ready, let us see how we can do regular heap things with it. But hey, are you not **curious** about how does it merge in $\mathcal{O}(\log{}n)$? I guess we will do this first, and you will see how the other operations come naturally with it.

So now we have two binomial heaps:

```{lang="dot"}

digraph G {
    node [shape="box"];
    a[color="red"]; b[color="blue"];
}
```

Pretend that you saw two binomial heaps. I was too lazy to generate the full thing. So, what we want to do is to merge binomial trees of the same size. If you have one of each in the heap, we can just merge the two like what is described above. If there is only one of each, we keep it the way it was. But what happens when we have two trees of the same size, and had previously created one that has the size? We merge the heaps arbitrarily. Wait doesn't this seem a bit familiar? If we have two heaps with binomial trees of degree at most 8, we can represent them in binaries, where the $i^th$ bit is 1 if there is a binomial tree of order i, 0 otherwise. Merging the two heaps $00111010_2$ and $01001001_2$ gives us $10000011_2$. This is like binary addition! It easily comes to us that the there are at most $\log{}N$ binomial trees in the heap, where $N$ is the number of elements in the heap. But then, how does one find the minimum element? We just keep a pointer to the root of the tree containing the minimum element, and whenever ```find_min``` is called, we return its value.

This marks the end of part 1. In the next part, I will explain how does insertion of elements work, as well as how deletion of elements work.
