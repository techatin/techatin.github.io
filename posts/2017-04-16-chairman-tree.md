---
title: Data Persistent Segment Tree
author: Techatin
tags: competitive programming, data structures
bios: It's China magic, or is it?
---

In Singapore NOI 2015, a rather peculiar question was asked, where the contestants
are supposed to perform a range $k^{th}$ largest query. The official solution presented
was a somewhat obscure merge sort segment tree, which had pretty high time and space
complexities. Of course, there are ways to do better than that. This article presents
such a data structure called data persistent segment tree(DPST), or known affectionately
by the Chinese OIers, 主席树(chairman's tree).

<!--more-->

Using a DPST to solve the aforementioned problem may not be intuitive at the first glance,
so we will start with the easiest case. Imagine that the query is on the whole range,
but you are only allowed to use a segment tree. The idea is to build a tree on the values
of the array. Each node $[l, r]$ stores the information how many elements have values between $l$
and $r$ inclusive. For instance, consider the following array:

$$ r = [1, 3, 2, 4, 5, 7, 6, 8] $$

The resultant segment tree will look something like:

```{lang="dot"}
digraph G {
    a -> b;
    a -> c;
    b -> d;
    b -> e;
    c -> f;
    c -> g;
    d -> h;
    d -> i;
    e -> j;
    e -> k;
    f -> l;
    f -> m;
    g -> n;
    g -> o;
    a [label="[1, 8]: 8"];
    b [label="[1, 4]: 4"];
    c [label="[5, 8]: 4"];
    d [label="[1, 2]: 2"];
    e [label="[3, 4]: 2"];
    f [label="[5, 6]: 2"];
    g [label="[7, 8]: 2"];
    h [label="[1, 1]: 1"];
    i [label="[2, 2]: 1"];
    j [label="[3, 3]: 1"];
    k [label="[4, 4]: 1"];
    l [label="[5, 5]: 1"];
    m [label="[6, 6]: 1"];
    n [label="[7, 7]: 1"];
    o [label="[8, 8]: 1"];
}
```

Where $[l, r]:k$ means there are $k$ values smaller than or equal to $r$ but larger than
or equal to $l$. The solution then comes almost naturally. If the label on the right subtree
is more than or equals to $k$, we know that the answer definitely lies somewhere in the segment
covered by the right subtree. Otherwise, it must be in the left subtree. More generally, the algorithm
can be described using the following pesudocode (actually c++):

```c++

int find_kth(node n, int k) {
    if (is_leaf(n)) return n.label;
    if (n.right_child.size >= k) return find_kth(n.right_child, k);
    else return find_kth(n.left_child, k - n.right_child.size());
}

```

But this is quite stupid - we can just sort first, or simply iterate through and get the same
result. You will see how it can extend to more complicated scenarios, such as the one below:

Assuming we now have to entertain all queries in the form $<1, i>: k$, that is, finding the $k^{th}$
largest element between positions 1 and $i$. Notice how the notation is different. We will use $[l, r]$
to denote the range of values, and $<l, r>$ to denote the range of positions in the given array. We can
use the same approach as above, building a segment tree for each range starting at index one. The only
problem is with the ridiculous space complexity of $\mathcal{O}(n^2\log{}n)$, where $n$ is the number of
distinct elements in the array.

Now, the magic will start. We will use the idea of data persistence to reduce the space complexity to $\mathcal{O}(n\log{}n)$. First, we note that every time we insert an element, at most one subtree for each node has changed. This means that we can just reuse the unchanged node. For instance, consider the array

$$ r = [1, 3, 2, 4, 5, 7, 6, 8] $$

At $t=1$, we insert the first element into our value-based segment tree. To avoid confusion, we color edges pointing to the left child red.

```{lang="dot"}
digraph G {
    a -> b [color=red];
    a -> c;
    b -> d [color=red];
    b -> e;
    c -> f [color=red];
    c -> g;
    d -> h [color=red];
    d -> i;
    e -> j [color=red];
    e -> k;
    f -> l [color=red];
    f -> m;
    g -> n [color=red];
    g -> o;
    a [label="[1, 8]: 1"];
    b [label="[1, 4]: 1"];
    c [label="[5, 8]: 0"];
    d [label="[1, 2]: 1"];
    e [label="[3, 4]: 0"];
    f [label="[5, 6]: 0"];
    g [label="[7, 8]: 0"];
    h [label="[1, 1]: 1"];
    i [label="[2, 2]: 0"];
    j [label="[3, 3]: 0"];
    k [label="[4, 4]: 0"];
    l [label="[5, 5]: 0"];
    m [label="[6, 6]: 0"];
    n [label="[7, 7]: 0"];
    o [label="[8, 8]: 0"];
}
```

At $t=2$, we create a new root node $[1, 8]: 2$. But notice that instead of creating both child nodes of the root node, we can reuse the right child since it has not been altered. We do the same for all subsequent nodes. To make things clearer, we colored all newly created nodes blue. We have:

```{lang="dot"}
digraph G {
    a -> b [color=red];
    a -> c;
    b -> d [color=red];
    b -> e;
    c -> f [color=red];
    c -> g;
    d -> h [color=red];
    d -> i;
    e -> j [color=red];
    e -> k;
    f -> l [color=red];
    f -> m;
    g -> n [color=red];
    g -> o;
    a1 -> c;
    a1 -> b1 [color=red];
    b1 -> d [color=red];
    b1 -> e1;
    e1 -> k;
    e1 -> j1 [color=red];
    a [label="[1, 8]: 1"];
    b [label="[1, 4]: 1"];
    c [label="[5, 8]: 0"];
    d [label="[1, 2]: 1"];
    e [label="[3, 4]: 0"];
    f [label="[5, 6]: 0"];
    g [label="[7, 8]: 0"];
    h [label="[1, 1]: 1"];
    i [label="[2, 2]: 0"];
    j [label="[3, 3]: 0"];
    k [label="[4, 4]: 0"];
    l [label="[5, 5]: 0"];
    m [label="[6, 6]: 0"];
    n [label="[7, 7]: 0"];
    o [label="[8, 8]: 0"];
    a1 [label="[1, 8]: 2", color=blue];
    b1 [label="[1, 4]: 2", color=blue];
    e1 [label="[3, 4]: 1", color=blue];
    j1 [label="[3, 3]: 1", color=blue];
}
```

We repeat this until we have inserted every element from our original array and keep track
of each root. Notice that in each insert, we only create one node at each layer, or $\mathcal{O}(\log{}n)$ nodes, and there
is a total of $n$ insertions. As such, our space complexity is only $\mathcal{O}(n\log{}n)$. After
all the insertions are done, we can locate the root at each $t$ in constant time using a look-up
table and query for the $k^{th}$ largest element from the start to a certain position in $\mathcal{O}(\log{}n)$. We are now ready to take the final step and solve the problem completely.

For the final step, we notice that we can find the number of elements in the range $[l, r]$ whose indices
falls in range $<t_1, t_2>$ by doing a prefix sum. That is, we find number of elements in the range $[l, r]$ in index-range $<1, t_2>$ and $<1, t_1-1>$ separately, then take their difference. A similar idea that solves the two subproblems can then be used again. The pseudocode (actually c++, again) below shows the query algorithm:

```c++

node nodes[N];

int find_kth(node n1, node n2, int k) {
    if(is_leaf(n)) return n.label;
    int t = n2.size() - n1.size();
    if(k <= t) return find_kth(n1.right_child, n2.right_child, t);
    else return find_kth(n1.left_child, n2.left_child, k - t);
}

int main() {
    // process the nodes
    int t1, t2, k;
    cin >> t1 >> t2 >> k;
    cout << find_kth(nodes[t1 - 1], nodes[t2], k);
}

```

The remaining code will be posted when I recovered from the trauma of writing this post and
the immense amount of homework. For any queries, feel free to email me at 18yluxu069i@students.ri.edu.sg.
