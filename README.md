# argyle

Arc + Guile

```scheme
(use (argyle))

(def sq (x) (* x x))
(let v #[10 20 30] (v 1))            ; => 20, data structures are callable
(#{'a 1 'b 2} 'b)                    ; => 2
(+ "ab" "cd")                        ; => "abcd"
(map (\\ * 2 _) #[1 2])              ; => #<<vec> v: #(2 4)>
(let (:keys a b) #{'a 1 'b 2} (+ a b)) ; => 3, destructuring everywhere
(aif (assq 'b '((b . 2))) (cdr it) 'none) ; => 2

(data point (x y))
(point-x (point 1 2))                ; => 1

(gen describe (fn (x) 'thing))
(xtnd describe (n <int>) 'int)
(describe 1)                         ; => int
```

## Running

Requires Guile 3.0 (`brew install guile`). `(argyle fibers)` also needs
guile-fibers (`brew install guile-fibers`).

```sh
bin/argyle                    # REPL with (argyle) loaded
bin/argyle path/to/file.scm   # run a script; start it with (use (argyle))
test/run                      # run all test suites
test/run base reader          # run some of them
```

`bin/argyle` puts the repo on Guile's load path and loads `boot.scm`,
which defines the `ns` / `use` module forms and turns on `:keyword`
syntax. Argyle sources can't load without it.

If you change a module's exports, clear the compile cache, because
`re-export-ns` snapshots exports at compile time:

```sh
rm -rf ~/.cache/guile/ccache/3.0-*/$(pwd)
```
