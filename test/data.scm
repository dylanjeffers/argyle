;;; Data structures: vec, table, queue, set, and user-defined data / data! types.
;;; Run with: test/run data

(use (argyle) (test check))

;;; vec: applicable, (v k) reads, (v k x) writes, (v) is the raw vector

(def v (vec 10 20 30))
(check "vec?" #t (vec? v))
(check "vec read" 20 (v 1))
(check "vec write" 99 (do (v 1 99) (v 1)))
(check "vec raw" #(10 99 30) (v))
(check "vec-length" 3 (vec-length v))
(check "vec->list" '(10 99 30) (vec->list v))
(check "list->vec" #(1 2) ((list->vec '(1 2))))
(check "make-vec" #(0 0) ((make-vec 2 0)))
(check "vec-copy is a copy" '(#(1 2) #(9 2))
       (let a (vec 1 2)
         (let b (vec-copy a)
           (a 0 9)
           (list (b) (a)))))
(check "vec-fill!" #(5 5) (let a (vec 1 2) (vec-fill! a 5) (a)))
(check "vec-map" #(2 4) ((vec-map (\\ * 2 _) (vec 1 2))))
(check "vec type" '<vec> (type v))

;;; table: applicable, (t k) reads, (t k x) writes and returns the table

(def t (table 'a 1 'b 2))
(check "table?" #t (table? t))
(check "table read" 2 (t 'b))
(check "table missing" #f (t 'zzz))
(check "table write returns table" #t (table? (t 'c 3)))
(check "table written" 3 (t 'c))
(check "table-count" 3 (table-count (const #t) t))
(check "table-delete!" #f (do (t 'd 4) (table-delete! t 'd) (t 'd)))
(check "table-fold" 6 (table-fold (fn (k v acc) (+ v acc)) 0 t))
(check "table-map->list" '(a b c)
       (sort (table-map->list (fn (k v) k) t) (fn (x y) (string<? (symbol->string x) (symbol->string y)))))
(check "table-for-each" 6 (let n 0 (table-for-each (fn (k v) (=! n (+ n v))) t) n))
(check "update" 11 (do (update t 'a (\\ + 10 _)) (t 'a)))
(check "table-clear!" 0 (do (table-clear! t) (table-count (const #t) t)))
(check "make-table empty" 0 (table-count (const #t) (make-table)))
(check "group-by" '((#f (1 3)) (#t (2 4)))
       (sort (group-by even? '(1 2 3 4)) (fn (a b) (~ (car a)))))
(check "table type" '<table> (type (table)))

;;; queue: applicable, (q x) enqueues, (q) dequeues

(def qq (queue 1 2))
(check "queue?" #t (queue? qq))
(check "queue enqueue" 3 (do (qq 3) (queue-length qq)))
(check "queue dequeue" 1 (qq))
(check "queue-peek" 2 (queue-peek qq))
(check "queue->list" '(2 3) (queue->list qq))
(check "enqueue! dequeue!" '(2 3 4) (do (enqueue! qq 4) (queue->list qq)))
(check "dequeue! order" '(2 3 4) (list (dequeue! qq) (dequeue! qq) (dequeue! qq)))
(check "queue-empty?" #t (queue-empty? qq))
(check-err "dequeue! empty" (dequeue! qq))
(check "make-queue" 0 (queue-length (make-queue)))
(check "queue-clear!" 0 (let qq (queue 1 2) (queue-clear! qq) (queue-length qq)))
(check "list->queue" '(1 2) (queue->list (list->queue '(1 2))))
(check "queue->vec vec->queue" '(#(1 2) (3 4))
       (list ((queue->vec (queue 1 2))) (queue->list (vec->queue (vec 3 4)))))

;;; set: applicable, (s x) adds

(def s (set 1 2 #f))
(check "set?" #t (set? s))
(check "has?" '(#t #t #f) (list (has? s 2) (has? s #f) (has? s 7)))
(check "set add" #t (do (s 9) (has? s 9)))
(check "elements" '(1 2 9) (sort (filter identity (elements s)) <))
(check "set-clear!" '() (let s (set 1 2) (set-clear! s) (elements s)))

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

(data! cell (val))
(def c (cell 1))
(check "trans setter mutates" 5 (do (cell-val! c 5) (cell-val c)))

;;; custom application with :app (self is the record)

(data counter (n) :app (fn () (counter-n self)))
(check "data :app" 7 ((counter 7)))
(data! acc (total) :app (fn (x) (acc-total! self (+ x (acc-total self))) self))
(check "trans :app" 6 (acc-total ((((acc 0) 1) 2) 3)))

;;; custom constructor with :init

(data pair2 (a b) :init (mke-pair2 a b))
(check "data :init" '(1 2) (let p (mke-pair2 1 2) (list (pair2-a p) (pair2-b p))))

(done)
