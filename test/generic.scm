;;; Generic functions: built-in generics over every type, and gen / xtnd
;;; for user-defined ones.  Run with: test/run generic

(use (argyle) (test check))

;;; Built-in generics

(check "len" '(3 5 2 2 2 3 2)
       (list (len '(1 2 3)) (len "hello") (len #[1 2]) (len #{'a 1 'b 2})
             (len (queue 1 2)) (len 123) (len (stream-range 0 2))))
(check "rev" '((3 2 1) "cba") (list (rev '(1 2 3)) (rev "abc")))
(check "join" '((1 2 3) "abc" (0 1 5)) (list (join '(1) '(2 3)) (join "a" "b" "c")
       (stream->list (join (stream-range 0 2) (stream-range 5 6)))))
(check "cpy" '((1 2) #(1 2)) (list (cpy '(1 2)) ((cpy #[1 2]))))
(check "cpy is a copy" '(1 2)
       (let a (list 1 2) (let b (cpy a) (set-car! a 9) b)))
(check "cpy queue" '(1 2) (queue->list (cpy (queue 1 2))))
(check "clr!" '((1) 0 0)
       (list (let l (list 1 2) (clr! l) l)
             (let t #{'a 1} (clr! t) (len t))
             (let qq (queue 1) (clr! qq) (len qq))))
(check "kth" '(b 20) (list (kth '(a b) 1) (kth #[10 20] 1)))
(check "map" '((2 4) #(2 4) "AB")
       (list (map (\\ * 2 _) '(1 2)) ((map (\\ * 2 _) #[1 2]))
             (map char-upcase "ab")))
(check "map table" '(1 2) (sort (map (fn (k v) v) #{'a 1 'b 2}) <))
(check "map several lists" '(4 6) (map + '(1 2) '(3 4)))
(check "str" '("a1b" "" "x") (list (str 'a 1 "b") (str) (str #\x)))
(check "type" '(<int> <str> <vec> <table> <queue> <gen-fn>)
       (list (type 1) (type "s") (type #[]) (type #{}) (type (queue)) (type len)))
(check "gen-fn?" '(#t #f) (list (gen-fn? len) (gen-fn? car)))

;;; Collection generics keep the collection's type where it makes sense

(def sorted (s) (sort (elements s) <))
(check "map queue set stream" '((2 4) (2 4) (2 4))
       (list (queue->list (map (\\ * 2 _) (queue 1 2)))
             (sorted (map (\\ * 2 _) (set 1 2)))
             (stream->list (map (\\ * 2 _) (stream-range 1 3)))))
(check "filter lst vec str" '((2 4) #(2 4) "ab")
       (list (filter even? '(1 2 3 4)) ((filter even? #[1 2 3 4]))
             (filter char-alphabetic? "a1b2")))
(check "filter table keeps entries" '((b . 2))
       (let t (filter (fn (k v) (> v 1)) #{'a 1 'b 2})
         (table-map->list cons t)))
(check "filter queue set stream" '((2) (2) (0 2 4))
       (list (queue->list (filter even? (queue 1 2 3)))
             (sorted (filter even? (set 1 2 3)))
             (stream->list (stream-take 3 (filter even? (stream-from 0))))))
(check "reduce" '(10 10 #\c 6 6 6 0)
       (list (reduce + 0 '(1 2 3 4)) (reduce + 0 #[1 2 3 4])
             (reduce (fn (c acc) (if (char>? c acc) c acc)) #\a "abc")
             (reduce + 0 (queue 1 2 3)) (reduce + 0 (set 1 2 3))
             (reduce + 0 (stream-range 1 4)) (reduce + 0 #[])))
(check "cpy str table set are copies" '("ab" 1 (1))
       (list (let s (string-copy "ab") (let c (cpy s) (string-set! s 0 #\z) c))
             (let t #{'a 1} (let c (cpy t) (t 'a 9) (c 'a)))
             (let s (set 1) (let c (cpy s) (s 2) (sorted c)))))
(check "clr! set" '() (let s (set 1 2) (clr! s) (elements s)))
(check "len set" 2 (len (set 1 2)))

;;; Anything applicable dispatches as <fn>

(check "generic fn as the mapped fn" '(1 2) (vec->list (map len #["a" "bb"])))
(check "vec as the mapped fn" '(b c) (map #['a 'b 'c] '(1 2)))
(check "table as a filter pred" '(a) (filter #{'a #t} '(a b)))

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

(gen tag-with)
(xtnd tag-with (t <any> n <int>) (list 'int t n))
(xtnd tag-with (t <any> s <str>) (list 'str t s))
(check "<any> matches every type" '((int a 1) (str 2 "s") (int "x" 3))
       (list (tag-with 'a 1) (tag-with 2 "s") (tag-with "x" 3)))

(gen pick)
(xtnd pick (x <int>) 'exact)
(xtnd pick (x <any>) 'any)
(check "exact type beats <any>" '(exact any) (list (pick 1) (pick "s")))

(data point (x y))
(xtnd describe (p <point>) (list 'point (point-x p)))
(check "dispatch on data type" '(point 1) (describe (point 1 2)))

(gen rev)
(xtnd rev (p <point>) (point (point-y p) (point-x p)))
(check "extend over existing fn" '((2 1) (3 2 1))
       (let p (rev (point 1 2)) (list (list (point-x p) (point-y p)) (rev '(1 2 3)))))

(done)
