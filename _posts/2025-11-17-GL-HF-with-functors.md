# GLHF with functors

If functional programming were a theme park, **Functors** would be the friendly ride operators who take your values, lift them gently into a context, and make sure any transformation you apply goes *smoothly and safely*.

In this post, we’re going to explore what functors are, why they matter, and how they relate to that mysterious big sibling of theirs: **Monads**. Don’t worry—we’ll keep it joyful, intuitive, and full of good vibes.

---

## What *Is* a Functor, Really?

At its heart, a **Functor** is just *something you can map over*.

Formally, a functor is a type that implements a function like:

\[
\text{map} : F(a) \to (a \to b) \to F(b)
\]

In plainer, more human terms:

> A functor takes a value wrapped in some kind of container (like `Maybe`, `List`, or `Promise`) and lets you apply a normal function to the wrapped value **without unwrapping it**.

Think of a functor as a **mailbox** holding a letter (your value). You don’t open the mailbox—you just tell the postal worker (the `map` function) how to transform the letter, and they’ll preserve the mailbox shape while updating the contents.

---

## Why Are Functors Useful?

Functors let you apply functions to values that aren’t *just* values anymore. They live in some computational context:

- `Maybe` → a value that might be missing  
- `List` → potentially many values  
- `Promise` → a value that will arrive in the future  
- `IO` → a value tied to side effects (in languages like Haskell)

Without functors, you’d constantly be unpacking, checking, rewrapping—ugh.
With functors, you write cleaner, more elegant pipelines of transformations.

---

## A Few Examples

### Mapping over a list

```js
[1, 2, 3].map(x => x + 1); 
// → [2, 3, 4]
````

### Mapping over a Maybe

```haskell
fmap (+1) (Just 2)  -- → Just 3
fmap (+1) Nothing   -- → Nothing
```

### Mapping over a Promise

```js
Promise.resolve(2).then(x => x + 1);
// → Promise(3)
```

Each example shows:
Same mapping function → different contexts → same functor magic.

---

## The Two Functor Laws (Don’t Worry—They’re Friendly)

Functors must follow two simple rules:

1. **Identity law**
   Mapping with the identity function does nothing.
   [
   \text{map}(\text{id}, F(x)) = F(x)
   ]

2. **Composition law**
   Mapping with two functions in sequence is the same as mapping with their composition.
   [
   \text{map}(g, \text{map}(f, F(x))) = \text{map}(g \circ f, F(x))
   ]

These keep functors predictable and composable—like good citizens of the functional world.

---

## Enter: Monads — Functors with Superpowers

If functors are the friendly ride operators, **monads** are the full-on adventure coordinators who say:

> “Not only can I map over your wrapped value—I can *flatten* nested contexts and help you chain computations that produce wrapped results.”

Technically, every monad is a functor.
Monads extend functors by adding two operations:

* `return` / `pure` — wrap a value in the monadic context
* `bind` (`>>=`) — apply a function that returns a monadic value and **flatten** the layers

Here’s a fun mental model:

| Concept     | Analogy                                                                             |
| ----------- | ----------------------------------------------------------------------------------- |
| **Functor** | “I can apply a function to my wrapped value.”                                       |
| **Monad**   | “I can apply functions that return wrapped values, and handle the nesting for you.” |

### A quick example

```haskell
-- Functor-style
fmap (+1) (Just 2)        -- → Just 3

-- Monad-style (binding a function that returns Maybe)
(Just 2) >>= (\x -> Just (x + 1))
```

Functors give us “apply inside the box.”
Monads give us “apply inside the box **and** handle new boxes.”

---

## Visualizing the Difference

```
Functor:
   [x] --map(f)--> [f(x)]

Monad:
   [x] --bind(f)--> f(x)  -- but f(x) is already in brackets!
       So the monad flattens: [[y]] → [y]
```

The flattening step turns nested contexts into a single context:

[
\text{join} : M(M(a)) \to M(a)
]

Monads = Functor + "join" + good intentions

---

## Wrapping Up

Functors give functional programming its expressive power by letting you apply transformations inside contexts without breaking their structure. They are everywhere: lists, options, futures, IO—you name it.

And once you’re comfortable with functors, monads feel far less mystical. They’re just functors that also know how to flatten and chain effects.

So next time you see something with a `map` function...

Go forth and map joyfully!
Functional programming doesn’t have to be scary—it can be *functor-tastic*.
