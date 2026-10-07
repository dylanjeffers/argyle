(ns (argyle base type)
  :export (base-type coerce))
(use (argyle base mac)
     (argyle base fn)
     (argyle base ctrl)
     (argyle base err)
     (argyle base ns)
     (argyle base type lst)
     (argyle base type str)
     (argyle base type num)
     (argyle base type fn)
     (argyle base type sym)
     (argyle base type syn)
     (argyle base type chr)
     (argyle base type kwd)
     (argyle base type strm)
     (argyle base type tup))

;;; TODO: add simple heirarchy
(def base-type (x)
 (cond
  ((lst? x)        '<lst>)
  ((tup? x)        '<tup>)
  ((str? x)        '<str>)
  ((int? x)        '<int>)
  ((num? x)        '<num>)
  ((fn? x)         '<fn>)
  ((sym? x)        '<sym>)
  ((syn? x)        '<syn>)
  ((strm? x)       '<strm>)
  ((hash-table? x) '<hash-tbl>)
  ((chr? x)        '<chr>)
  ((vector? x)     '<vector>)
  ((kwd? x)        '<kwd>)
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
       (<str> (<int> ,num->str)
              (<num> ,num->str)
              (<chr> ,string)
              (<sym> ,(fn (x) (if (eqv? x (symbol)) "" (sym->str x)))))
       
       (<sym> (<str> ,str->sym)
              (<chr> ,(fn (c) (str->sym (string c))))
              (<num> ,(\\ (compose str->sym num->str) _)))
       
       (<int> (<chr> ,(fn (c . args) (chr->int c)))
              (<num> ,(fn (x . args) (iround x)))
              (<str> ,(fn (x . args)
                        (aif (str->num x) (iround it)
                             (error "Can't coerce" x '-> 'int)))))
       
       (<num> (<str> ,(fn (x . args)
                        (or (str->num x)
                            (error "Can't coerce " x '-> 'num))))
              (<int> ,(fn (x) x)))
       
       (<chr> (<int> ,int->chr)
              (<num> ,(fn (x) (int->chr
                               (iround x)))))))
    coercions))

(def iround (compose inexact->exact round))

(mac export-type-ctrs
  ((t1 ...)
   #`(do #,@(map (fn (t)
                   #`(defp #,t (obj . args)
                       (apply coerce obj
                              (sym-join '< '#,t '>) args)))
                 #'(t1 ...)))))

(export-type-ctrs str num int sym syn dat chr)

(re-export-ns 
 (argyle base type lst)
 (argyle base type str)
 (argyle base type num)
 (argyle base type fn)
 (argyle base type sym)
 (argyle base type syn)
 (argyle base type chr)
 (argyle base type kwd)
 (argyle base type strm)
 (argyle base type tup))
