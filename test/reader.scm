;;; Reader extensions: #[...] vec, #{...} tbl, #@ deref, #~.
;;; Run with: test/run reader

(use (argyle) (test check))

(check "#[] reads as vec" #t (vec? #[1 2 3]))
(check "#[] contents" #(1 2 3) (#[1 2 3]))
(check "#[] applicable" 2 (#[1 2 3] 1))
(check "#[] empty" #() (#[]))
(check "#[] evaluates elements" #(3 x) (#[(+ 1 2) 'x]))
(check "#[] symbols and numbers" #(1.5 -2) (#[1.5 -2]))
(check "#[] strings" #("a" "b") (#["a" "b"]))
(check "#[] nested" 3 ((#[1 #[2 3]] 1) 1))
(check "#[] quoted symbol last" #(1 x) (#[1 'x]))
(check "#[] quasiquoted symbol last" #(y) (#[`y]))

(check "#{} reads as table" #t (table? #{'a 1}))
(check "#{} lookup" 2 (#{'a 1 'b 2} 'b))
(check "#{} evaluates" 3 (#{'k (+ 1 2)} 'k))
(check "#{} string keys" 1 (#{"a" 1} "a"))
(check "#{} quoted symbol value last" 'v (#{1 'v} 1))
(check "#{} holding #[]" 20 ((#{'v #[10 20]} 'v) 1))

(check "#@ derefs a ref" 9 (let r (ref 9) #@r))
(check "#@ derefs a future" 4 (let f (futr (* 2 2)) #@f))
(check "#~ reads as (~ x)" '(~ x) '#~x)
(check "#~ negates" #t #~#f)

(done)
