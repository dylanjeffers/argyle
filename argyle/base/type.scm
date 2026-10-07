(ns (argyle base type)
  :export (base-type coerce))
(use (argyle base mac)
     (argyle base fn)
     (argyle base ctrl)
     (argyle base err)
     (argyle base ns)
     (argyle base type lst)
     (argyle base type syn)
     (argyle base type strm))

;;; TODO: add simple heirarchy
(def base-type (x)
 (cond
  ((list? x)        '<lst>)
  ((pair? x)        '<tup>)
  ((string? x)        '<str>)
  ;; Not two clauses: Guile 3.0.11's compiler wrongly skips a number?
  ;; clause that follows a failed integer? test, so 1.5 fell through.
  ((number? x)     (if (integer? x) '<int> '<num>))
  ((procedure? x)         '<fn>)
  ((symbol? x)        '<sym>)
  ((syn? x)        '<syn>)
  ((strm? x)       '<strm>)
  ((hash-table? x) '<hash-tbl>)
  ((char? x)        '<chr>)
  ((vector? x)     '<vector>)
  ((keyword? x)        '<kwd>)
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
              (<lst> ,syn->dat))
       ;; So clearly this is a bit hacky
       (<syn> (<lst> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<num> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<str> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<sym> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<kwd> ,(fn (dat ctx) (dat->syn ctx dat))))
       (<str> (<int> ,number->string)
              (<num> ,number->string)
              (<chr> ,string)
              (<sym> ,(fn (x) (if (eqv? x (symbol)) "" (symbol->string x)))))
       
       (<sym> (<str> ,string->symbol)
              (<chr> ,(fn (c) (string->symbol (string c))))
              (<num> ,(\\ (compose string->symbol number->string) _)))
       
       (<int> (<chr> ,(fn (c . args) (char->integer c)))
              (<num> ,(fn (x . args) (iround x)))
              (<str> ,(fn (x . args)
                        (aif (string->number x) (iround it)
                             (error "Can't coerce" x '-> 'int)))))
       
       (<num> (<str> ,(fn (x . args)
                        (or (string->number x)
                            (error "Can't coerce " x '-> 'num))))
              (<int> ,(fn (x) x)))
       
       (<chr> (<int> ,integer->char)
              (<num> ,(fn (x) (integer->char
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

(export-type-ctrs str num int sym syn dat chr)

(re-export-ns
 (argyle base type lst)
 (argyle base type syn)
 (argyle base type strm))
