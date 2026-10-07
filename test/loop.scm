;;; foof-loop as used from argyle code: loop, its clauses, and the
;;; nested-loop collect / iterate / recur forms.  The upstream suite for
;;; loop itself is in foof-loop.scm.  Run with: test/run loop

(use (argyle) (test check))

;;; loop

(check "listing" '(1 4 9)
       (loop ((for x (in-list '(1 2 3))) (for acc (listing (* x x)))) => acc))
(check "summing" 6 (loop ((for x (in-list '(1 2 3))) (for s (summing x))) => s))
(check "multiplying" 24 (loop ((for x (up-from 1 (to 5))) (for p (multiplying x))) => p))
(check "maximizing minimizing" '(9 1)
       (loop ((for x (in-list '(3 9 1))) (for hi (maximizing x)) (for lo (minimizing x)))
         => (list hi lo)))
(check "where accumulates" 6 (loop ((for x (in-list '(1 2 3))) (where s 0 (+ s x))) => s))
(check "until stops early" '(1 2)
       (loop ((for x (in-list '(1 2 3 4))) (until (> x 2)) (for acc (listing x))) => acc))
(check "up-from by" '(0 2 4)
       (loop ((for i (up-from 0 (to 6) (by 2))) (for acc (listing i))) => acc))
(check "down-from" '(3 2 1)
       (loop ((for i (down-from 4 (to 1))) (for acc (listing i))) => acc))
(check "in-vector in-string" '((1 2) (#\a #\b))
       (list (loop ((for x (in-vector #(1 2))) (for acc (listing x))) => acc)
             (loop ((for c (in-string "ab")) (for acc (listing c))) => acc)))
(check "in-lists" '(4 6)
       (loop ((for xs (in-lists '((1 2) (3 4)))) (for acc (listing (apply + xs)))) => acc))
(check "listing with filter" '(2 4)
       (loop ((for x (in-list '(1 2 3 4))) (for acc (listing x (if (even? x))))) => acc))
(check "appending" '(1 2 3)
       (loop ((for l (in-list '((1) (2 3)))) (for acc (appending l))) => acc))
(check "named loop" 10
       (loop lp ((x 0) (n 0)) (if (< n 5) (lp (+ x n) (1+ n)) x)))
(check "in-port" '(a b)
       (call-with-input-string "a b"
         (fn (port) (loop ((for d (in-port port read)) (for acc (listing d))) => acc))))

;;; nested-loop

(check "collect-list nested" '((1 . a) (1 . b) (2 . a) (2 . b))
       (collect-list (for x (in-list '(1 2))) (for y (in-list '(a b)))
         (cons x y)))
(check "collect-sum" 6 (collect-sum (for x (in-list '(1 2 3))) x))
(check "collect-product" 6 (collect-product (for x (in-list '(1 2 3))) x))
(check "collect-count" 2 (collect-count (for x (in-list '(1 2 3))) (if (odd? x))))
(check "collect-vector" #(1 4) (collect-vector (for x (in-list '(1 2))) (* x x)))
(check "collect-string" "AB" (collect-string (for c (in-string "ab")) (char-upcase c)))
(check "collect-maximum" 3 (collect-maximum (for x (in-list '(1 3 2))) x))
(check "collect-average" 2 (collect-average (for x (in-list '(1 2 3))) x))
(check "collect-stream" '(2 4)
       (strm->lst (collect-stream (for x (in-list '(1 2))) (* 2 x))))
(check "iterate" 6 (iterate ((s 0)) + (for x (in-list '(1 2 3))) x))
(check "recur" '(1 2 3) (recur '() cons (for x (in-list '(1 2 3))) x))

(done)
