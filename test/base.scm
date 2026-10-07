;;; (argyle base): functions, control flow, arithmetic, io, types, macros.
;;; Run with: test/run base

(use (argyle) (test check))

;;; Definitions and functions

(def sq (x) (* x x))
(def k 5)
(check "def fn" 9 (sq 3))
(check "def value" 5 k)
(check "fn" 5 ((fn (a b) (+ a b)) 2 3))
(check "fn rest" '(2 3) ((fn (a . rst) rst) 1 2 3))
(check "fn optional default" 10 ((fn (a :o (b :or 9)) (+ a b)) 1))
(check "fn optional given" 3 ((fn (a :o (b :or 9)) (+ a b)) 1 2))
(check "fns dispatch on arity" '(0 1 3)
       (let f (fns (() 0) ((a) a) ((a b) (+ a b)))
         (list (f) (f 1) (f 1 2))))
(check "let" 6 (let x 3 (* x 2)))
(check "w/ binds in sequence" 3 (w/ (a 1 b (+ a 1)) (+ a b)))
(check "ret" '(1) (ret x '() (=! x (cons 1 x))))
(check "\\\\ cut" '(2 3 4) (map (\\ + 1 _) '(1 2 3)))
(check "\\\\ rest slot" '(1 2 3) ((\\ list 1 ___) 2 3))
(check "->> threads left to right" 4 (->> 1 1+ (\\ * 2 _)))
(check "compose" 7 ((compose 1+ (\\ * 2 _)) 3))
(check "apply" 6 (apply + '(1 2 3)))
(check "wrap" 7 ((wrap 7) 'ignored))
(check "defined?" '(#t #f) (list (defined? 'sq) (defined? 'no-such-thing)))
(inline twice (x) (* 2 x))
(check "inline" 8 (twice 4))

;;; Control

(check "do" 2 (do 1 2))
(check "=! sets" 5 (let x 1 (=! x 5) x))
(check "=! sets several" '(1 2) (w/ (a 0 b 0) (=! a 1 b 2) (list a b)))
(check "= is numeric equality" '(#t #f #t) (list (= 1 1.0) (= 1 2) (= 2 2 2)))
(check "aif true" 2 (aif (assq 'b '((a . 1) (b . 2))) (cdr it) 'none))
(check "aif false" 'none (aif #f it 'none))
(check "&" '(2 #f) (list (& 1 2) (& 1 #f 3)))
(check "~" '(#t #f) (list (~ #f) (~ 1)))
(check "0? 1? =?" '(#t #t #f #t) (list (0? 0) (1? 1) (1? 2) (=? 2 2)))
(check "nil?" '(#t #f) (list (nil? '()) (nil? '(1))))
(check "flat-map" '(1 1 2 2) (flat-map (fn (x) (list x x)) '(1 2)))
(check "&map" '(#t #f) (list (&map odd? '(1 3)) (&map odd? '(1 2))))
(check "set\\\\" '(1 3) (set\ eqv? '(1 2 3) '(2)))
(check "values call-with-values" 3 (call-with-values (fn () (values 1 2)) +))
(check "call/ec escapes" 'out (call/ec (fn (k) (k 'out) 'not-here)))
(check "call/cc" 3 (+ 1 (call/cc (fn (k) (k 2)))))
(check "$> plain" 3 ($> (+ 1 2)))
(check "$> composable continuation" 12 ($> (+ 1 (abort (fn (k) (k (k 10)))))))
(check "$> escape" 'escaped ($> (* 2 (abort (fn (k) 'escaped)))))

;;; Arithmetic, overloaded on strings and symbols

(check "+ nums" 6 (+ 1 2 3))
(check "+ none" 0 (+))
(check "+ strs" "ab" (+ "a" "b"))
(check "+ str chr" "ab" (+ "a" #\b))
(check "+ syms" 'ab (+ 'a 'b))
(check "* nums" 24 (* 2 3 4))
(check "* none" 1 (*))
(check "* str repeats" "ababab" (* "ab" 3))
(check "* sym repeats" 'xx (* 'x 2))
(check "^" 8 (^ 2 3))
(check "positive? negative?" '(#t #t #f) (list (positive? 1) (negative? -1) (positive? -1)))
(check "length is polymorphic" '(3 2 2 1)
       (list (length '(1 2 3)) (length "ab") (length #(1 2))
             (let h (make-hash-table) (hash-set! h 'a 1) (length h))))

;;; IO

(check-out "print" "a1" (print "a" 1))
(check-out "println" "ab\n" (println "a" "b"))
(check-out "print-lines" "a\nb\n" (print-lines "a" "b"))
(check-out "format prints" "x=1\n" (format "x=~a\n" 1))
(check-out "pretty-print" "(1 2)\n" (pretty-print '(1 2)))

;;; Errors

(check-err "error raises" (error "boom" 1))
(check "error is misc-error" 'misc-error
       (catch #t (fn () (error "boom")) (fn (key . _) key)))

;;; Types and conversions

(check "base-type" '(<int> <num> <str> <sym> <lst> <chr> <fn>)
       (map base-type (list 1 1.5 "s" 'a '(1) #\c car)))
(check "coerce str->num" 42 (coerce "42" '<num>))
(check "coerce num->int rounds" 5 (coerce 4.6 '<int>))
(check "coerce int->str" "42" (coerce 42 '<str>))
(check "coerce same type" "s" (coerce "s" '<str>))
(check-err "coerce unknown target" (coerce "x" '<nope>))
(check "predicates" '(#t #t #t #t #t #t #t #t #t)
       (list (str? "") (sym? 'a) (num? 1.5) (int? 2) (chr? #\a)
             (fn? car) (kwd? :a) (lst? '(1)) (tup? '(1 . 2))))
(check "syn?" '(#t #f) (list (syn? #'x) (syn? 'x)))
(check "str conversions" '(12 abc "abc" (#\a #\b) "12")
       (list (str->num "12") (str->sym "abc") (sym->str 'abc)
             (str->lst "ab") (num->str 12)))
(check "chr conversions" '(97 #\a) (list (chr->int #\a) (int->chr 97)))
(check "kwd conversions" '(a #:b) (list (kwd->sym :a) (sym->kwd 'b)))
(check "str ops" '("abc" "ab" "c" 3 "ABC")
       (list (str-join "a" "bc") (str-take "abc" 2) (str-drop "abc" 2)
             (str-len "abc") (str-map char-upcase "abc")))
(check "sym-join" 'ab (sym-join 'a 'b))
(check "lst ops" '((1 2) #t #t 2 (1) (2 3) 1)
       (list (lst 1 2) (lst? '()) (empty? '()) (lst: '(1 2 3) 1)
             (lst-hd '(1 2 3) 1) (lst-tl '(1 2 3) 1) (lst-idx even? '(1 2 3))))
(check "unique range" '((1 2 3) (0 1 2)) (list (unique '(1 2 1 3)) (range 3)))
(check "filter reduce" '((2 4) 10)
       (list (filter even? '(1 2 3 4)) (reduce + 0 '(1 2 3 4))))
(check "chrs" '(#t #f) (list (chrs-has? (chrs #\a #\b) #\a)
                             (chrs-has? (chrs #\a) #\z)))
(check "fn-name" 'sq (fn-name sq))

;;; Macros

(mac swap!
  ((a b) #'(let tmp a (=! a b) (=! b tmp))))
(check "mac" '(2 1) (w/ (x 1 y 2) (swap! x y) (list x y)))

(mac kw-test (:to)
  ((x :to y) #'(list 'to x y))
  ((x y) #'(list 'plain x y)))
(check "mac keyword literal" '((to 1 2) (plain 1 2))
       (list (kw-test 1 :to 2) (kw-test 1 2)))

(mac only-ids
  ((x) (id? #'x) #''identifier)
  ((x) #''other))
(check "mac fender" '(identifier other) (list (only-ids foo) (only-ids 1)))

(mac count-args x
  ((arg ...) (datum->syntax x (length #'(arg ...)))))
(check "mac with context" 3 (count-args a b c))

(mac sum-tmps
  ((e ...)
   (w/syn ((t ...) (gen-tmps #'(e ...)))
     #'(let-values (((t) e) ...) (+ t ...)))))
(check "w/syn gen-tmps" 6 (sum-tmps 1 2 3))

(done)
