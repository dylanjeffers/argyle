;;; Data structures: vec, tbl, q, set, and user-defined data / trans types.
;;; Run with: test/run data

(use (argyle) (argyle data set) (test check))

;;; vec: applicable, (v k) reads, (v k x) writes, (v) is the raw vector

(def v (vec 10 20 30))
(check "vec?" #t (vec? v))
(check "vec read" 20 (v 1))
(check "vec write" 99 (do (v 1 99) (v 1)))
(check "vec raw" #(10 99 30) (v))
(check "vec: vec!" 7 (do (vec! v 0 7) (vec: v 0)))
(check "vec-len" 3 (vec-len v))
(check "vec->lst" '(7 99 30) (vec->lst v))
(check "lst->vec" #(1 2) ((lst->vec '(1 2))))
(check "mke-vec" #(0 0) ((mke-vec 2 0)))
(check "vec-cpy is a copy" '(#(1 2) #(9 2))
       (let a (vec 1 2)
         (let b (vec-cpy a)
           (a 0 9)
           (list (b) (a)))))
(check "vec-fill!" #(5 5) (let a (vec 1 2) (vec-fill! a 5) (a)))
(check "vec-map" #(2 4) ((vec-map (\\ * 2 _) (vec 1 2))))
(check "vec type" '<vec> (type v))

;;; tbl: applicable, (t k) reads, (t k x) writes and returns the table

(def t (tbl 'a 1 'b 2))
(check "tbl?" #t (tbl? t))
(check "tbl read" 2 (t 'b))
(check "tbl missing" #f (t 'zzz))
(check "tbl write returns tbl" #t (tbl? (t 'c 3)))
(check "tbl written" 3 (t 'c))
(check "tbl: tbl!" 4 (do (tbl! t 'd 4) (tbl: t 'd)))
(check "tbl-cnt" 4 (tbl-cnt (const #t) t))
(check "tbl-del!" #f (do (tbl-del! t 'd) (t 'd)))
(check "tbl-fold" 6 (tbl-fold (fn (k v acc) (+ v acc)) 0 t))
(check "tbl-map->lst" '(a b c)
       (sort (tbl-map->lst (fn (k v) k) t) (fn (x y) (string<? (sym->str x) (sym->str y)))))
(check "tbl-each" 6 (let n 0 (tbl-each (fn (k v) (= n (+ n v))) t) n))
(check "update" 11 (do (update t 'a (\\ + 10 _)) (t 'a)))
(check "tbl-clr!" 0 (do (tbl-clr! t) (tbl-cnt (const #t) t)))
(check "mke-tbl empty" 0 (tbl-cnt (const #t) (mke-tbl)))
(check "tblq" 'x (let h (mke-tbl) (tblq! h 'k 'x) (tblq: h 'k)))
(check "grp-by" '((#f (1 3)) (#t (2 4)))
       (sort (grp-by even? '(1 2 3 4)) (fn (a b) (~ (car a)))))
(check "tbl type" '<tbl> (type (tbl)))

;;; q: applicable, (q x) enqueues, (q) dequeues

(def qq (q 1 2))
(check "q?" #t (q? qq))
(check "q enqueue" 3 (do (qq 3) (q-len qq)))
(check "q dequeue" 1 (qq))
(check "q-pk" 2 (q-pk qq))
(check "q->lst" '(2 3) (q->lst qq))
(check "enq! deq!" '(2 3 4) (do (enq! qq 4) (q->lst qq)))
(check "deq! order" '(2 3 4) (list (deq! qq) (deq! qq) (deq! qq)))
(check "q-nil?" #t (q-nil? qq))
(check-err "deq! empty" (deq! qq))
(check "mke-q" 0 (q-len (mke-q)))
(check "lst->q" '(1 2) (q->lst (lst->q '(1 2))))
(check "q->vec vec->q" '(#(1 2) (3 4))
       (list ((q->vec (q 1 2))) (q->lst (vec->q (vec 3 4)))))

;;; set: applicable, (s x) adds

(def s (set 1 2 #f))
(check "set?" #t (set? s))
(check "has?" '(#t #t #f) (list (has? s 2) (has? s #f) (has? s 7)))
(check "set add" #t (do (s 9) (has? s 9)))
(check "elements" '(1 2 9) (sort (filter identity (elements s)) <))

;;; data: immutable records, functional setters

(data point (x y))
(def p (point 1 2))
(check "data pred" '(#t #f) (list (point? p) (point? 5)))
(check "data accessors" '(1 2) (list (point-x p) (point-y p)))
(check "data setter is functional" '(9 1) (list (point-x (point-x! p 9)) (point-x p)))
(check "data?" '(#t #f) (list (data? p) (data? 5)))
(check "data-type names the type" (quote <point>) (data-type p))
(check "data-type?" #t (data-type? <point>))
(check "data type" '<point> (type p))
(check-err "data not applicable" (p 1))

;;; trans: mutable records

(trans cell (val))
(def c (cell 1))
(check "trans setter mutates" 5 (do (cell-val! c 5) (cell-val c)))

;;; custom application with :app (self is the record)

(data counter (n) :app (fn () (counter-n self)))
(check "data :app" 7 ((counter 7)))
(trans acc (total) :app (fn (x) (acc-total! self (+ x (acc-total self))) self))
(check "trans :app" 6 (acc-total ((((acc 0) 1) 2) 3)))

;;; custom constructor with :init

(data pair2 (a b) :init (mke-pair2 a b))
(check "data :init" '(1 2) (let p (mke-pair2 1 2) (list (pair2-a p) (pair2-b p))))

(done)
