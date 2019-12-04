# Op

Op provides operators for writing easier-to-read Haskell. It provides new
operators with a consistent "look and feel" including fixity direction and
precedence, resulting in easier- and quicker-to-read code especially when used
on long chains of expressions.

To be easier to remember it reuses several common Haskell operators, but
redefines the fixities of a few of the lesser-used ones to be more consistently
composable. These conflicting ones are all non-Prelude, so you only need to
hide stuff when importing those extra modules. They are listed below.

For detailed documentation see the module documentation.

## Summary

Op              | Base          | Conflicts with Base?
--------------- | ------------- | --------------------
`x \|> f`       | `x & f`       | N/A
`f <\| x`       | `f $ x`       | N/A
`f .> g`        | `g . f`       | N/A
`g <. f`        | `g . f`       | N/A
`f >>> g`       | `f >>> g`     | Y, `Control.Category.>>>` is `infixr 1`
`g <<< f`       | `g <<< f`     | N, reused `Control.Category.<<<`
`x >$> f`       | `x <&> f`     | N/A
`f <$< x`       | `f <$> x`     | N/A
`f >=> g`       | `f >=> g`     | Y, `Control.Monad.>=>` is `infixr 1`
`g <=< f`       | `g <=< f`     | N, reused `Control.Monad.<=<`
`x >>= f`       | `x >>= f`     | N, reused `Prelude.>>=`
`f =<< x`       | `f =<< x`     | N, reused `Prelude.=<<`
