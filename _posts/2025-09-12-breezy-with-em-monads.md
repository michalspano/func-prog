---
title: Breezy with 'em Monads
mathjax: true
layout: post
categories: haskell, monads
---

Monads may have a reputation for being the “philosophers” of functional programming—but when it comes to **parsing**, they’re surprisingly practical, elegant, and even a little magical.  
Today, let’s take a pleasant stroll through **monadic parsers** and **monad transformers**, and see how they make parsing logic clean, composable, and expressive.

---

## Monadic Parsers: Tiny Functions with Big Charm

A **monadic parser** is simply a function that takes some input (usually a string) and returns a parsed value *plus* the remaining input.

\[
\text{Parser } a \equiv \text{String} \to (a, \text{String})
\]

But the real beauty shines when this parser is a **Monad**.

### Why Make Parsers Monadic?

Because monads let us chain parsers one after another in a clean, intuitive sequence.  
Each parser consumes part of the input and passes along the leftovers. No manual state wrangling. No tangled conditionals.

```

parserA >>= \x ->
parserB >>= \y ->
parserC >>= \z ->
return (x, y, z)

````

Instead of a giant tangle of code, you get a readable flow:

> “First read a letter, then read a number, then read a symbol.”

### A Simple Example (Haskell style)

```haskell
digit :: Parser Char
letter :: Parser Char

pair :: Parser (Char, Char)
pair = do
  d <- digit
  l <- letter
  return (d, l)
````

The monad handles input flow automatically: if `digit` fails, the whole chain fails.
No extra plumbing required.

---

## Monad Transformers: Power-Ups for Your Parsers

Sometimes a parser needs more than just input state.
It may need:

* Error messages with context
* Logging
* Configuration
* Backtracking control
* Accumulated metadata

This is where **monad transformers** shine. They layer capabilities on top of your base parser monad, creating a flexible and extensible effect stack.

A transformer wraps a monad:

[
\text{StateT } s; m; a \equiv s \to m (a, s)
]

You can mix and match:

* `StateT` for additional parsing state
* `ExceptT` for richer error messages
* `WriterT` for logs
* `ReaderT` for shared environment

A parser might become:

[
\text{ParserT } a \equiv \text{StateT String (ExceptT Error IO)}; a
]

This gives you input handling, error management, and IO all at once, while keeping the parser code succinct.

### Example: Adding Error Context

```haskell
type Parser a = StateT String (Either String) a

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  input <- get
  case input of
    (c:cs) | p c -> put cs >> return c
    _ -> lift (Left "Unexpected character")
```

The transformer stack handles both state and errors, allowing the parsing logic to remain focused and readable.
