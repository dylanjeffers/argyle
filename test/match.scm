;;; Pattern matching: match, destructuring fn / def / let / w/, fn-case,
;;; :keys, :or, :as.  Run with: test/run match

(use (argyle) (test check))

(check "match list" 3 (match '(1 2) ((a b) (+ a b))))
(check "match fallthrough" 'other (match 5 ((a b) 'pair) (_ 'other)))
(check "match literal" 'one (match 1 (1 'one) (_ 'other)))
(check "match rest" '(2 3) (match '(1 2 3) ((a . rst) rst)))
(check "match ellipsis" '(1 2 3) (match '(1 2 3) ((xs ...) xs)))
(check "match quasi" 'got (match '(op 1) (('op n) 'got) (_ 'no)))
(check "match pred" 'num (match 5 ((? number?) 'num) (_ 'no)))
(check-err "match no clause" (match 5 ((a b) 'pair)))

(check "fn destructures" '(1 2 3) ((fn ((x y) z) (list x y z)) '(1 2) 3))
(check "fn nested" 3 ((fn ((a (b c))) c) '(1 (2 3))))
(check "fn rest" '(2 3) ((fn (a . rst) rst) 1 2 3))
(check "fn :as" '(1 (1 2)) ((fn (((a b) :as whole)) (list a whole)) '(1 2)))
(check "fn :or default" '(1 9) ((fn (a :o (b :or 9)) (list a b)) 1))
(check "fn :keys" 3 ((fn ((:keys a b)) (+ a b)) #{'a 1 'b 2}))
(check-err "fn pattern mismatch" ((fn ((a b)) a) '(1 2 3)))

(def head ((h . _)) h)
(check "def destructures" 1 (head '(1 2)))
(def swap ((a b)) (list b a))
(check "def destructures list" '(2 1) (swap '(1 2)))

(check "let destructures" 3 (let (a b) '(1 2) (+ a b)))
(check "let :keys" 3 (let (:keys a b) #{'a 1 'b 2} (+ a b)))
(check "w/ several patterns" 6 (w/ ((a b) '(1 2) (c) '(3)) (+ a b c)))
(check "w/ sees earlier bindings" 4 (w/ (a 1 (b c) (list a 3)) (+ b c)))

(def f (fn-case ((a) (list 'one a)) ((a b) (list 'two a b))))
(check "fn-case" '((one 1) (two 1 2)) (list (f 1) (f 1 2)))
(def g (fn-case (((a b)) (list b a)) ((x y) 'two)))
(check "fn-case destructures" '((2 1) two) (list (g '(1 2)) (g 1 2)))
(check-err "fn-case picks by arity, not pattern" (g 5))

(done)
