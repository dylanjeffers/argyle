;;; Generic functions: built-in generics over every type, and gen / xtnd
;;; for user-defined ones.  Run with: test/run generic

(use (argyle) (test check))

;;; Built-in generics

(check "len" '(3 5 2 2 2 3 2)
       (list (len '(1 2 3)) (len "hello") (len #[1 2]) (len #{'a 1 'b 2})
             (len (q 1 2)) (len 123) (len (strm-range 0 2))))
(check "rev" '((3 2 1) "cba") (list (rev '(1 2 3)) (rev "abc")))
(check "join" '((1 2 3) "abc" (0 1 5)) (list (join '(1) '(2 3)) (join "a" "b" "c")
       (strm->lst (join (strm-range 0 2) (strm-range 5 6)))))
(check "cpy" '((1 2) #(1 2)) (list (cpy '(1 2)) ((cpy #[1 2]))))
(check "cpy is a copy" '(1 2)
       (let a (list 1 2) (let b (cpy a) (set-car! a 9) b)))
(check "cpy q" '(1 2) (q->lst (cpy (q 1 2))))
(check "clr!" '((1) 0 0)
       (list (let l (list 1 2) (clr! l) l)
             (let t #{'a 1} (clr! t) (len t))
             (let qq (q 1) (clr! qq) (len qq))))
(check "kth" '(b 20) (list (kth '(a b) 1) (kth #[10 20] 1)))
(check "map" '((2 4) #(2 4) "AB")
       (list (map (\\ * 2 _) '(1 2)) ((map (\\ * 2 _) #[1 2]))
             (map char-upcase "ab")))
(check "map tbl" '(1 2) (sort (map (fn (k v) v) #{'a 1 'b 2}) <))
(check "map several lists" '(4 6) (map + '(1 2) '(3 4)))
(check "str" '("a1b" "" "x") (list (str 'a 1 "b") (str) (str #\x)))
(check "type" '(<int> <str> <vec> <tbl> <q> <gen-fn>)
       (list (type 1) (type "s") (type #[]) (type #{}) (type (q)) (type len)))
(check "gen-fn?" '(#t #f) (list (gen-fn? len) (gen-fn? car)))

;;; User-defined generics

(gen describe (fn (x) 'thing))
(xtnd describe (n <int>) 'int)
(xtnd describe (s <str>) 'str)
(check "gen default" 'thing (describe 'sym))
(check "xtnd dispatch" '(int str) (list (describe 1) (describe "s")))

(gen combine)
(xtnd combine (a <int> b <int>) (+ a b))
(xtnd combine (a <str> b <int>) (* a b))
(check "dispatch on two args" '(3 "abab") (list (combine 1 2) (combine "ab" 2)))
(check-err "no method and no default" (combine 'x 'y))

(gen total)
(xtnd total (x <int> . rest) (apply + x rest))
(check "dispatch with rest args" '(1 6) (list (total 1) (total 1 2 3)))

(data point (x y))
(xtnd describe (p <point>) (list 'point (point-x p)))
(check "dispatch on data type" '(point 1) (describe (point 1 2)))

(gen rev)
(xtnd rev (p <point>) (point (point-y p) (point-x p)))
(check "extend over existing fn" '((2 1) (3 2 1))
       (let p (rev (point 1 2)) (list (list (point-x p) (point-y p)) (rev '(1 2 3)))))

(done)
