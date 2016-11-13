(define-module (arguile data queue)
  #:export (make-q q?
            q-len queue-null?
            enq! deq!
            queue-empty-condition?
            lst->q q->lst))

(use-modules (arguile ssyntax)
             (arguile core)
             (arguile generic)
             (arguile data)
             (rnrs conditions))

(data queue (len head tail)
      ((len q-len q-len!)
       (head q-head q-head!)
       (tail q-tail q-tail!))

      (case-lambda
        (() (deq! self))
        ((k) (enq! self k))))

(def make-q ()
  (make-queue 0 '() '()))

(def q-null? (q) (zero? (q-len q)))

(def enq! (q obj)
  (q-tail! q (cons obj (q-tail q)))
  (q-len! q (1+ (q-len q))))

(def deq! (q)
  (when (q-null? q)
    ;; Clean this verbose exp
    (raise (condition
            (make-queue-empty-condition)
            (make-who-condition 'dequeue)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list queue)))))
  (with (h (q-head q)
         t (q-tail q))
    (q-len! q (1- (q-len q)))
    (if (null? h)
        (let h* (reverse t)
          (q-head! q (cdr h*))
          (q-tail! q '())
          (car h*))
        (do
          (q-head! q (cdr h))
          (car h)))))

(define (lst->q lst)
  (_make-queue (len lst) lst '()))

(define (q->lst q)
  (+ (q-head q)
     (reverse (q-tail q))))

(define-condition-type &queue-empty
  &assertion
  make-queue-empty-condition
  queue-empty-condition?)
