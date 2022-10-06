## Learning Haskell here...

Use `cabal test` to run the test suite.

To get faster feedback, use this method:

Step 1: start a testing REPL with `cabal repl haskell-playground-test` in Tmux's **first** pane.
Step 2: run `./manage/reload_repl_on_change` in a second pane.


### Comparisons

#### Elixir

```elixir
shift = fn(list) -> Enum.map(list, fn(x) -> x + 1 end) end

String.upcase( List.to_string( shift.('ab') ))
# => "BCD"

'abc' |> shift.() |> List.to_string |> String.upcase
# => "BCD"
```

#### Haskell

```haskell
shift  = map succ
upcase = map toUpper

(shift . upcase) "abc"
-- => "BCD"

shift . upcase $ "abc"
-- => "BCD"

(map succ) . (map toUpper) $ "abc"
-- => "BCD"

map succ . map toUpper $ "abc"
-- => "BCD"

shift . shift . upcase $ "abc"
-- => "CDE"
```

```haskell
shiftn n = foldr (.) id (replicate n shift)

shiftn 1 . upcase $ "abc"
-- => "BCD"
shiftn 2 . upcase $ "abc"
-- => "CDE"
shiftn 13 . upcase $ "abc"
-- => "NOP"
```

#### Elm

```elm
upcase = Char.toUpper
shift x = Char.fromCode <| Char.toCode(x) + 1

"abc" |> String.toList |> List.map(\c -> c |> shift |> upcase) |> String.fromList
-- => "BCD"
"abc" |> String.toList |> List.map(\c -> c |> succ |> succ |> upcase) |> String.fromList
-- => "CDE"
```

#### Conclusion

In Haskell, you "describe" the computation by combining functions with the dot operator,
then you apply this computation by passing the argument at the end.

In Elixir and Elm, you can pass the argument at the start of the chain, and keep on chaining the
previous function's output into the next function's input.

So whereas in Elixir and Elm you "keep on chaining" at the end, in Haskell you "keep on composing" at the start.


### Int vs Integer

```
Integer :: *
Defined in ‘GHC.Integer.Type’ (integer-gmp-1.0.3.0)

Arbitrary precision integers. In contrast with fixed-size integral types such as Int , the Integer type represents the entire infinite range of integers.

For more information about this type's representation, see the comments in its implementation.

---

Int :: *
Defined in ‘GHC.Types’ (ghc-prim-0.6.1)

A fixed-precision integer type with at least the range [-2^29 .. 2^29-1] .
The exact range for a given implementation can be determined by using Prelude.minBound and
Prelude.maxBound from the Prelude.Bounded class.
```