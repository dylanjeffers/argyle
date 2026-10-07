(ns (argyle base type)
  :export (base-type coerce))
(use (argyle base mac)
     (argyle base fn)
     (argyle base ctrl)
     (argyle base err)
     (argyle base ns)
     (argyle base type lst)
     (argyle base type syn)
     (argyle base type stream))

;;; TODO: add simple heirarchy
(def base-type (x)
 (cond
  ((list? x)        '<list>)
  ((pair? x)        '<pair>)
  ((string? x)        '<str>)
  ;; Not two clauses: Guile 3.0.11's compiler wrongly skips a number?
  ;; clause that follows a failed integer? test, so 1.5 fell through.
  ((number? x)     (if (integer? x) '<int> '<number>))
  ((procedure? x)         '<fn>)
  ((symbol? x)        '<symbol>)
  ((syn? x)        '<syn>)
  ((stream? x)       '<stream>)
  ((hash-table? x) '<hash-table>)
  ((char? x)        '<char>)
  ((vector? x)     '<vector>)
  ((keyword? x)        '<keyword>)
  ((null? x)       '<nil>)
  (else            (error "Type: unknown type" x))))

(def coerce (x to-type . args)
  (let x-type (base-type x)
    (if (eqv? to-type x-type) x
        (w/ (conversions (hash-ref coercions to-type)
             converter (and conversions (hash-ref conversions x-type)))
          (if converter
              (apply converter (cons x args))
              (error "Can't coerce" x '-> to-type))))))

(def coercions
  (ret coercions (make-hash-table)
    (for-each
     (fn (e)
       (w/ (target-type (car e)
            conversions (make-hash-table))
         (hash-set! coercions target-type conversions)
         (for-each
          (fn (x) (hash-set! conversions (car x) (cadr x)))
          (cdr e))))
     `((<dat> (<syn> ,syn->dat)
              (<list> ,syn->dat))
       ;; So clearly this is a bit hacky
       (<syn> (<list> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<number> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<str> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<symbol> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<keyword> ,(fn (dat ctx) (dat->syn ctx dat))))
       (<str> (<int> ,number->string)
              (<number> ,number->string)
              (<char> ,string)
              (<symbol> ,symbol->string))
       
       (<symbol> (<str> ,string->symbol)
              (<char> ,(fn (c) (string->symbol (string c))))
              (<number> ,(\\ (compose string->symbol number->string) _)))
       
       (<int> (<char> ,(fn (c . args) (char->integer c)))
              (<number> ,(fn (x . args) (iround x)))
              (<str> ,(fn (x . args)
                        (aif (string->number x) (iround it)
                             (error "Can't coerce" x '-> '<int>)))))
       
       (<number> (<str> ,(fn (x . args)
                        (or (string->number x)
                            (error "Can't coerce" x '-> '<number>))))
              (<int> ,(fn (x) x)))
       
       (<char> (<int> ,integer->char)
              (<number> ,(fn (x) (integer->char
                               (iround x)))))))
    coercions))

(def iround (compose inexact->exact round))

(mac export-type-ctrs
  ((t1 ...)
   #`(do #,@(map (fn (t)
                   #`(defp #,t (obj . args)
                       (apply coerce obj
                              (symbol-append '< '#,t '>) args)))
                 #'(t1 ...)))))

(export-type-ctrs str number int syn dat char)

;;; Guile's symbol joins strings into a symbol; keep that, and coerce
;;; anything else.
(defp symbol args
  (if (or (null? args) (string? (car args)))
      (string->symbol (apply string-append args))
      (apply coerce (car args) '<symbol> (cdr args))))

(re-export-ns
 (argyle base type lst)
 (argyle base type syn)
 (argyle base type stream))
