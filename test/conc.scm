;;; Concurrency: futures, refs, transactions.  Run with: test/run conc

(use (argyle) (test check))

(check "futr @" 42 (@ (futr (* 6 7))))
(check "futr?" '(#t #f) (list (futr? (futr 1)) (futr? 1)))
(check "futures run concurrently" '(1 2 3)
       (map @ (list (futr 1) (futr 2) (futr 3))))
(check "doasync" '(2 4) (call-with-values (fn () (doasync (+ 1 1) (+ 2 2))) list))

(check "ref @" 5 (@ (ref 5)))
(check "ref?" '(#t #f) (list (ref? (ref 1)) (ref? 1)))
(check-err "@ on a non-lazy value" (@ 5))

(def r (ref 1))
(dosync (r) (alter r (1+ (@ r))))
(check "dosync alter" 2 (@ r))

(def a (ref 10))
(def b (ref 0))
(dosync (a b)
  (alter a (- (@ a) 3))
  (alter b (+ (@ b) 3)))
(check "dosync several refs" '(7 3) (list (@ a) (@ b)))

(def counter (ref 0))
(def bump () (dosync (counter) (alter counter (1+ (@ counter)))))
(for-each @ (map (fn (i) (futr (bump))) (iota 50)))
(check "dosync from many futures" 50 (@ counter))

(done)
