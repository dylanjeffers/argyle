;;; Tests for (argyle monad), using the examples from its docstrings.
;;; Run with: test/run monad

(use (argyle) (argyle monad) (test check))

(def state-val (mval :o (state :or '()))
  (c/vals (fn () (run-w/state mval state)) list))

(check "ident return" 5 (w/monad ident-monad (return 5)))
(check "ident >>=" 6 (w/monad ident-monad
                       (>>= (return 5) (fn (x) (return (1+ x))))))
(check "n-ary >>=" '(3 ())
       (state-val (w/monad state-monad
                    (>>= (return 1)
                         (lift 1+ state-monad)
                         (lift 1+ state-monad)))))
(check "mlet*" '(3 ())
       (state-val (mlet* state-monad ((a (return 1))
                                      (b -> 2))
                    (return (+ a b)))))
(check "mlet" '((1 2) ())
       (state-val (mlet state-monad ((a (return 1))
                                     (b (return 2)))
                    (return (list a b)))))
(check "mdo" '(2 (x))
       (state-val (mdo state-monad (state-push 'x) (return 2))))
(check "foldm" '((c b a) ())
       (state-val (foldm state-monad (lift2 cons state-monad) '() '(a b c))))
(check "mapm" '((1 2 3) ())
       (state-val (mapm state-monad (lift1 1+ state-monad) '(0 1 2))))
(check "anym" '(#t ())
       (state-val (anym state-monad (lift1 odd? state-monad) '(0 1 2))))
(check "seq" '((1 2) ())
       (state-val (seq state-monad (list (return 1) (return 2)))))
(check "listm" '((1 2) ())
       (state-val (listm state-monad (return 1) (return 2))))
(check "state push/pop" '(b (a))
       (state-val (mdo state-monad (state-push 'a) (state-push 'b) (state-pop))))
(check "curr-state" '((s) (s))
       (state-val (curr-state) '(s)))
(check "mwhen" '(x (x))
       (state-val (w/monad state-monad
                    (mwhen #t (state-push 'x) (return 'x)))))
(check "monad?" #t (monad? state-monad))

(done)
