(ns (argyle base mac)
    :export (mac mac? syn-case let-syn w/syn
                  syn-param w/syn-params gen-tmps))
(use (argyle guile)
     (ice-9 receive))

(define-syntax mac
  (lambda (x)
    (define (ids? exps)
      (and-map identifier? exps))
    (define (lits? exps)
      (and-map (lambda (e) (or (identifier? e) (keyword? (syntax->datum e))))
               exps))
    (syntax-case x ()
      ((_ name ctx (f1 ...) exp ...)
       (and (ids? `(,#'name ,#'ctx))
            (lits? #'(f1 ...)))
       #'(%mac name ctx (f1 ...) exp ...))
      ((_ name ctx exp ...)
       (ids? `(,#'name ,#'ctx))
       #'(mac name ctx () exp ...))
      ((_ name (f1 ...) exp ...)
       (lits? #'(f1 ...))
       #'(mac name ctx (f1 ...) exp ...))
      ((_ name exp ...)
       (identifier? #'name)
       #'(mac name ctx () exp ...)))))

(define-syntax %mac
  (lambda (x)
    (syntax-case x ()
      ((_ name ctx (f1 ...) exp ...)
       #`(define-syntax name
           (lambda (ctx)
             #,@(receive (defs cases) (parse-mac #'(exp ...))
                  (if (null? cases) defs
                      #`(#,@defs (syntax-case ctx #,(literals #'(f1 ...))
                                   #,@(format cases)))))))))))

(eval-when (compile load eval)
  ;; Guile 3 only accepts identifiers as syntax-case literals; keyword
  ;; literals like :init still match as plain datums, so drop them here.
  (define (literals lits)
    (filter identifier? lits))

  (define (format cases)
    (map (lambda (tmp case)
           (syntax-case case ()
             (((patt ... . rst) . rest)
              #`(#,(cons tmp #'(patt ... . rst)) . rest))))
         (generate-temporaries cases)
         cases))

  (define (parse-mac exps)
    (let lp ((exps exps) (defs '()) (patts '()))
      (if (null? exps) (values (reverse defs) (reverse patts))
          (if (patt? (car exps))
              (lp (cdr exps) defs (cons (car exps) patts))
              (lp (cdr exps) (cons (car exps) defs) patts)))))

  (define (patt? exp)
    (syntax-case exp ()
      (((patt ... . rst) (guard-exp ...) ... templ) #t)
      (_ #f))))

(mac mac?
  ((mac) #'(macro? (module-ref (current-module) 'mac))))

(mac syn-case
  ((ctx (aux ...) ((patt ... . rst) templ) ...)
   #`(syntax-case ctx #,(literals #'(aux ...))
       ((patt ... . rst) templ) ...)))

(mac let-syn
  ((syn exp body ...)
   #'(w/syn (syn exp) body ...)))

(mac w/syn
  (((item ...) e1 ...)
   (with-syntax ((items (grp #'(item ...) 2)))
     #'(with-syntax items e1 ...))))

(mac syn-param
  ((name fn) #'(define-syntax-parameter name fn)))

;;; TODO: change to w/ format
(mac w/syn-params
  ((((param val) ...) body ...)
   #'(syntax-parameterize ((param val) ...) body ...)))

(mac gen-tmps
  ((syn) #'(generate-temporaries syn)))
