---
title: Regular Expressions
author: Techatin
tags: Computational Theory, Random
bios: The good, the bad, the fun
---

Some people, when confronted with a problem, think
“I know, I'll use regular expressions.” Now they have two problems.

<!--more-->

Regular expressions, or Regexes, have a terrible reputation of being hard to understand, learn and implement. Seriously, just looking at it feels bad enough:

    <([A-Z][A-Z0-9]*)\b[^>]*>(.*?)</\1>

But there is actually quite a lot of Mathematics going on behind this thing. In my opinion, knowing the math will get you a long way in trying to understand Regex.

Before we start with Regex, let's look at something called finite state automatons(FSA). FSAs can be understood as a set of rules between a set of states. There are two special states, the **start** state, where we start from, and the **final** state, which we end up in. For instance, suppose you want to buy a can of drink that costs \$0.50 from a vending machine. The machine accepts coins of values \$0.10, \$0.20, \$0.50, and \$1.00. Then, the decision process of the machine can actually be represented using an FSA! Consider the following FSA:




```{lang="dot"}
digraph G {
    a->b[label="$0.10"];
    a->c[label="$0.20"];
    a->f[label="$0.50"];
    b->c[label="$0.10"];
    b->d[label="$0.20"];
    b->g[label="$0.50"];
    c->d[label="$0.10"];
    c->e[label="$0.20"];
    c->h[label="$0.50"];
    d->e[label="$0.10"];
    d->f[label="$0.20"];
    d->i[label="$0.50"];
    e->f[label="$0.10"];
    e->g[label="$0.20"];
    e->j[label="$0.50"];
    f->k;
    g->k;
    h->k;
    i->k;
    j->k;
    a[label="$0.00"];
    b[label="$0.10"];
    c[label="$0.20"];
    d[label="$0.30"];
    e[label="$0.40"];
    f[label="$0.50"];
    g[label="change $0.10"];
    h[label="change $0.20"];
    i[label="change $0.30"];
    j[label="change $0.40"];
    k[label="Drink!!!"];

}
```

So essentially, the FSA just 'remembers' the amount of money that we had put in, and gives us the drink and changes once we had put in more than \$0.50. Note that this machine is quite massive now, though it certainly gets more complicated in the real life. Now you may have some intuition on what an FSA is. I think you are now ready to take the mathematics part.

Mathematically speaking, an FSA is a quintuple $(\sum, S, s_0, \delta, F)$, where

- $\sum$ is the set of alphabet that the machine accepts. That is, what type of thing it can actually read.
- $S$ is the set of states
- $s_0$ is the start state, that alll actions start in
- $\delta$ is the transition function $\delta: \sum \times S \rightarrow S$ that states what state you will end up in given the current state and input.
- $F$ is the set of final states. If a sequence of input lands us in the final state, we say that the FSA **accepts** the input

