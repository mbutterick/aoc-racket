## MB’s Advent of Code tips

* Complex numbers are a nice way of modeling two-dimensional positions.

* Use lists whenever feasible, because there are many useful list functions in the Racket library that don’t have vector equivalents.

* In particular, [these list functions](https://docs.racket-lang.org/reference/pairs.html?q=racket%2Flist#%28part._.Additional_.List_.Functions_and_.Synonyms%29) are very useful.

* `eq?` is the fastest equality check, but it only works for symbols and fixnums (therefore, use more symbols and fixnums so you can use `eq?`!)

* `match` is fantastic.

* Association lists (= lists of pairs) can be used with the dictionary forms (like `dict-ref` and `in-dict`)

* The `graph` library can be useful for graph-based problems.

## My solutions

* I try to write solutions that are succinct but not cryptic. 

* I don’t optimize for speed.

* I like doing the Advent of Code problems because it forces me to use parts of Racket that I don’t ordinarily use. So I treat it as a chance to expand my awareness of the Racketverse.
