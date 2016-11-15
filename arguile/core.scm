(module (arguile core)
  #:export (fn def let with do fn-case & = =? 0? 1?))
(use (arguile guile)
     (arguile ssyntax)
     (arguile loop)
     (arguile type)
     (arguile io)
     (arguile generic))

(mac fn
  ((_ args e1 e2 ...)
   #'(lambda args e1 e2 ...)))

(mac def x
  ((_ name (arg1 ... . rest) e1 e2 ...)
   #`(#,@(if (has-kwargs? #'(arg1 ...))
             #`(define* (name #,@(expand-kwargs #'(arg1 ...) x) . rest))
             #'(define (name arg1 ... . rest)))
      e1 e2 ...))
  ((_ name val)
   #'(define name val)))

(mac let
  ((_ var val e1 e2 ...)
   #'(with (var val) e1 e2 ...)))

(mac with
  ((_ () e1 e2 ...)
   #'(_let () e1 e2 ...))
  ((_ (var val) e1 e2 ...)
   #'(_let ((var val)) e1 e2 ...))
  ((_ (var val rest ...) e1 e2 ...)
   #'(_let ((var val)) (with (rest ...) e1 e2 ...))))

(mac do ((_ e1 ...) #'(begin e1 ...)))
;;; TODO: check if var is a free variable, and if so, define it
(mac =
  ((_ var val) #'(set! var val))
  ((_ var val rest ...)
   #'(do (set! var val)
       (= rest ...))))

(mac fn-case
  ((_ fn1 fn2 ...) #'(case-lambda fn1 fn2 ...)))

(mac &
  ((_ e1 ...) #'(and e1 ...)))

;;; Make generic
(def =? _=)
(def 0? zero?)
(def 1? (n) (=? 1 n))

(def expand-kwargs (args ctx)
  (loop ((for arg (in-list args))
         (where args* '()
           (cons (let arg* (-> dat arg)
                   (if (& (keyword? arg*) (eq? #:o arg*))
                       (dat->syn ctx #:optional)
                       arg))
                 args*)))
        => (rev args*)
        ))

(def has-kwargs? (args)
  (or-map (fn (arg) (keyword? (syntax->datum arg)))
          args))
