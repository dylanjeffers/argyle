(ns (argyle base generic)
  :replace (+ * length))
(use (argyle guile)
     (argyle base fn)
     (argyle base type)) 

(def + args
  (cond ((null? args) 0)
        ((number? (car args)) (apply _+ args))
        ((one-of `(,string? ,char?) (car args))
         (apply string-append (map str args)))
        ((symbol? (car args))
         (apply symbol-append (map symbol args)))
        (else (apply _+ args))))

;;; TODO: Add cartesian product for data
(def * args
  (cond ((null? args) 1)
        ((number? (car args)) (apply _* args))
        ((one-of `(,string? ,char?) (car args))
         (apply string-append
                (map (fn (val) (str (car args)))
                     (iota (apply _* (cdr args))))))
        ((symbol? (car args))
         (apply symbol-append
                (map (fn (val) (symbol (car args)))
                     (iota (apply _* (cdr args))))))
        (else (apply _* args))))

(def length (x)
  (cond ((list? x) (_length x))
        ((string? x) (string-length x))
        ((hash-table? x) (hash-count (const #t) x))
        ((vector? x) (vector-length x))
        (else (_length x))))

(def one-of (tests val)
  (if (null? tests) #f
      (or ((car tests) val)
          (one-of (cdr tests) val))))
