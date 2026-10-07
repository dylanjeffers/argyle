(ns (argyle base fn)
    :replace (fn fns def defp let ret w/ \\ ->> inline id? wrap))
(use (argyle guile)
     (argyle base mac)
     (ice-9 receive))

(mac def x
  ((name (arg ... . rst) e1 e2 ...)
   #`(define* (name #,@(expand-kwargs #'(arg ...) x) . rst)
       e1 e2 ...))
  ((name val) #'(define name val)))

(mac fn x
  (((arg ... . rst) body ...)
   #`(lambda* (#,@(expand-kwargs #'(arg ...) x) . rst)
       body ...)))

(def fn-xpnd (fn-exp)
  (syn-case fn-exp ()
    (((arg ... . rst) body ...)
     #`((#,@(expand-kwargs #'(arg ...) fn-exp) . rst)
        body ...))))

(mac fns
  ((f1 f2 ...)
   #`(case-lambda*
      #,@(map fn-xpnd #'(f1 f2 ...)))))

(mac defp
  ((name . rest) #'(begin (def name . rest) (export name))))

(mac let
  ((var val e1 e2 ...) #'(w/ (var val) e1 e2 ...))
  ((() e1 e2 ...) #'(_let () e1 e2 ...)))

(mac ret ((var e1 e2 ...) #'(let var e1 e2 ... var)))

(mac w/
  ((() e1 e2 ...)
   #'(_let () e1 e2 ...))
  (((var val) e1 e2 ...)
   (if (id? #'var)
       #'(_let ((var val)) e1 e2 ...)
       #'(receive var val e1 e2 ...)))
  (((var val rest ...) e1 e2 ...)
   (if (id? #'var)
       #'(_let ((var val)) (w/ (rest ...) e1 e2 ...))
       #'(receive var val  (w/ (rest ...) e1 e2 ...)))))

(mac \\ ((fn args ...) #'(cut fn args ...)))

;;; Replace w/ => when loop export is handled
(mac ->>
  ((exp f ...) #`((compose #,@(reverse #'(f ...))) exp)))

(mac inline
  ((name (arg ...) body ...)
   #'(define-inlinable (name arg ...) body ...)))

(defp id? identifier?)
(defp wrap const)
