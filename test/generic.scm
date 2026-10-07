;;; Generic functions: built-in generics over every type, and generic / extend
;;; for user-defined ones.  Run with: test/run generic

(use (argyle) (test check))

;;; Built-in generics

(check "length" '(3 5 2 2 2 3 2)
       (list (length '(1 2 3)) (length "hello") (length #[1 2]) (length #{'a 1 'b 2})
             (length (queue 1 2)) (length 123) (length (stream-range 0 2))))
(check "reverse" '((3 2 1) "cba") (list (reverse '(1 2 3)) (reverse "abc")))
(check "join" '((1 2 3) "abc" (0 1 5)) (list (join '(1) '(2 3)) (join "a" "b" "c")
       (stream->list (join (stream-range 0 2) (stream-range 5 6)))))
(check "copy" '((1 2) #(1 2)) (list (copy '(1 2)) ((copy #[1 2]))))
(check "copy is a copy" '(1 2)
       (let a (list 1 2) (let b (copy a) (set-car! a 9) b)))
(check "copy queue" '(1 2) (queue->list (copy (queue 1 2))))
(check "clear!" '((1) 0 0)
       (list (let l (list 1 2) (clear! l) l)
             (let t #{'a 1} (clear! t) (length t))
             (let qq (queue 1) (clear! qq) (length qq))))
(check "nth" '(b 20) (list (nth '(a b) 1) (nth #[10 20] 1)))
(check "map" '((2 4) #(2 4) "AB")
       (list (map (\\ * 2 _) '(1 2)) ((map (\\ * 2 _) #[1 2]))
             (map char-upcase "ab")))
(check "map table" '(1 2) (sort (map (fn (k v) v) #{'a 1 'b 2}) <))
(check "map several lists" '(4 6) (map + '(1 2) '(3 4)))
(check "str" '("a1b" "" "x") (list (str 'a 1 "b") (str) (str #\x)))
(check "type" '(<int> <str> <vec> <table> <queue> <generic>)
       (list (type 1) (type "s") (type #[]) (type #{}) (type (queue)) (type length)))
(check "generic?" '(#t #f) (list (generic? length) (generic? car)))

(check "length raw vector and hash table" '(3 1)
       (list (length #(1 2 3)) (let h (make-hash-table) (hash-set! h 'a 1) (length h))))
(check "first rest" '((1 (2 3)) (1 #f) (7) (0 1))
       (list (list (first '(1 2 3)) (rest '(1 2 3)))
             (list (first #[1 2]) (vec? (rest '(1))))
             (list (first (queue 7 8)))
             (list (first (stream-range 0 3)) (first (rest (stream-range 0 3))))))
(check "take drop" '((a b) (c) (0 1) (5 6))
       (list (take '(a b c) 2) (drop '(a b c) 2)
             (stream->list (take (stream-range 0 9) 2))
             (stream->list (take (drop (stream-from 0) 5) 2))))
(check "nth" '(b 20 5) (list (nth '(a b) 1) (nth #[10 20] 1) (nth (stream-from 0) 5)))

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
(check "copy str table set are copies" '("ab" 1 (1))
       (list (let s (string-copy "ab") (let c (copy s) (string-set! s 0 #\z) c))
             (let t #{'a 1} (let c (copy t) (t 'a 9) (c 'a)))
             (let s (set 1) (let c (copy s) (s 2) (sorted c)))))
(check "clear! set" '() (let s (set 1 2) (clear! s) (elements s)))
(check "length set" 2 (length (set 1 2)))

;;; Anything applicable dispatches as <fn>

(check "generic fn as the mapped fn" '(1 2) (vec->list (map length #["a" "bb"])))
(check "vec as the mapped fn" '(b c) (map #['a 'b 'c] '(1 2)))
(check "table as a filter pred" '(a) (filter #{'a #t} '(a b)))

;;; User-defined generics

(generic describe (fn (x) 'thing))
(extend describe (n <int>) 'int)
(extend describe (s <str>) 'str)
(check "generic default" 'thing (describe 'sym))
(check "extend dispatch" '(int str) (list (describe 1) (describe "s")))

(generic combine)
(extend combine (a <int> b <int>) (+ a b))
(extend combine (a <str> b <int>) (* a b))
(check "dispatch on two args" '(3 "abab") (list (combine 1 2) (combine "ab" 2)))
(check-err "no method and no default" (combine 'x 'y))

(generic total)
(extend total (x <int> . rest) (apply + x rest))
(check "dispatch with rest args" '(1 6) (list (total 1) (total 1 2 3)))

(generic tag-with)
(extend tag-with (t <any> n <int>) (list 'int t n))
(extend tag-with (t <any> s <str>) (list 'str t s))
(check "<any> matches every type" '((int a 1) (str 2 "s") (int "x" 3))
       (list (tag-with 'a 1) (tag-with 2 "s") (tag-with "x" 3)))

(generic pick)
(extend pick (x <int>) 'exact)
(extend pick (x <any>) 'any)
(check "exact type beats <any>" '(exact any) (list (pick 1) (pick "s")))

(data point (x y))
(extend describe (p <point>) (list 'point (point-x p)))
(check "dispatch on data type" '(point 1) (describe (point 1 2)))

(generic reverse)
(extend reverse (p <point>) (point (point-y p) (point-x p)))
(check "extend over existing fn" '((2 1) (3 2 1))
       (let p (reverse (point 1 2)) (list (list (point-x p) (point-y p)) (reverse '(1 2 3)))))

(done)
