(ns (argyle data records)
  :replace (record record? record-type?)
  :export (define-record-type))
(use (srfi srfi-1)
     (system base ck))

;; A vtable-vtable whose instances are applicable vtables, so records
;; built from it can be called like procedures (their first field holds
;; the procedure).  Guile has no public constructor for this, so set the
;; applicable-vtable flag (1 << 2) on the vtable's flags slot directly.
(define (make-app-vtable fields printer)
  (let ((vt (make-vtable fields printer)))
    (struct-set!/unboxed vt 1 (logior (struct-ref/unboxed vt 1) 4))
    vt))

;; 0: type-name, 1: fields, 2: constructor
(define record-type-vtable
  (let ((s (make-app-vtable (string-append standard-vtable-fields "pwpwpw")
                            (lambda (s p)
                              (display "#<data-type " p)
                              (display (record-type-name s) p)
                              (display ">" p)))))
      (set-struct-vtable-name! s 'data-type)
    s))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define* (record-constructor rtd #:optional field-names)
  (if (not field-names)
      (struct-ref rtd (+ 2 vtable-offset-user))
      (primitive-eval
       `(lambda ,field-names
          (make-struct ',rtd 0 ,@(map (lambda (f)
                                        (if (memq f field-names)
                                            f
                                            #f))
                                      (record-type-fields rtd)))))))
          
(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (%record-type-error rtd obj)  ;; private helper
  (or (eq? rtd (record-type-descriptor obj))
      (scm-error 'wrong-type-arg "%record-type-check"
                 "Wrong type record (want `~S'): ~S"
                 (list (record-type-name rtd) obj)
                 #f)))

(define (record-accessor rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj)
      (if (eq? (struct-vtable obj) rtd)
          (struct-ref obj pos)
          (%record-type-error rtd obj)))))

(define (record-modifier rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj val)
      (if (eq? (struct-vtable obj) rtd)
          (struct-set! obj pos val)
          (%record-type-error rtd obj)))))

(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

;; Roll our own instead of using the public `define-inlinable'.  This is
;; because the public one has a different `make-procedure-name', so
;; using it would require users to recompile code that uses SRFI-9.  See
;; <http://lists.gnu.org/archive/html/guile-devel/2011-04/msg00111.html>.
;;

(define-syntax-rule (define-inlinable (name formals ...) body ...)
  (define-tagged-inlinable () (name formals ...) body ...))

;; 'define-tagged-inlinable' has an additional feature: it stores a map
;; of keys to values that can be retrieved at expansion time.  This is
;; currently used to retrieve the rtd id, field index, and record copier
;; macro for an arbitrary getter.

(define-syntax-rule (%%on-error err) err)

(define %%type #f)   ; a private syntax literal
(define-syntax getter-type
  (syntax-rules (quote)
    ((_ s 'getter 'err)
     (getter (%%on-error err) %%type s))))

(define %%index #f)  ; a private syntax literal
(define-syntax getter-index
  (syntax-rules (quote)
   ((_ s 'getter 'err)
    (getter (%%on-error err) %%index s))))

(define %%copier #f) ; a private syntax literal
(define-syntax getter-copier
  (syntax-rules (quote)
   ((_ s 'getter 'err)
    (getter (%%on-error err) %%copier s))))

(define-syntax define-tagged-inlinable
  (lambda (x)
    (define (make-procedure-name name)
      (datum->syntax name
                     (symbol-append '% (syntax->datum name)
                                    '-procedure)))

    (syntax-case x ()
      ((_ ((key value) ...) (name formals ...) body ...)
       (identifier? #'name)
       (with-syntax ((proc-name  (make-procedure-name #'name))
                     ((args ...) (generate-temporaries #'(formals ...))))
         #`(begin
             (define (proc-name formals ...)
               body ...)
             (define-syntax name
               (lambda (x)
                 (syntax-case x (%%on-error key ...)
                   ((_ (%%on-error err) key s) #'(ck s 'value)) ...
                   ((_ args ...)
                    #'((lambda (formals ...)
                         body ...)
                       args ...))
                   ((_ a (... ...))
                    (syntax-violation 'name "Wrong number of arguments" x))
                   (_
                    (identifier? x)
                    #'proc-name))))))))))

(define (default-record-printer s p)
  (display "#<" p)
  (display (record-type-name (record-type-descriptor s)) p)
  (let loop ((fields (cdr (record-type-fields (record-type-descriptor s))))
             (off 1))
    (cond
     ((not (null? fields))
      (display " " p)
      (display (car fields) p)
      (display ": " p)
      (write (struct-ref s off) p)
      (loop (cdr fields) (+ 1 off)))))
  (display ">" p))

(define-syntax-rule (throw-bad-struct s who)
  (let ((s* s))
    (throw 'wrong-type-arg who
           "Wrong type argument: ~S" (list s*)
           (list s*))))

(define (make-copier-id type-name)
  (datum->syntax type-name
                 (symbol-append '%% (syntax->datum type-name)
                                '-set-fields)))

(define-syntax %%set-fields
  (lambda (x)
    (syntax-case x ()
      ((_ type-name (getter-id ...) check? s (getter expr) ...)
       (every identifier? #'(getter ...))
       (let ((copier-name (syntax->datum (make-copier-id #'type-name)))
             (getter+exprs #'((getter expr) ...))
             (nfields (length #'(getter-id ...))))
         (define (lookup id default-expr)
           (let ((results
                  (filter (lambda (g+e)
                            (free-identifier=? id (car g+e)))
                          getter+exprs)))
             (case (length results)
               ((0) default-expr)
               ((1) (cadar results))
               (else (syntax-violation
                      copier-name "duplicate getter" x id)))))
         (for-each (lambda (id)
                     (or (find (lambda (getter-id)
                                 (free-identifier=? id getter-id))
                               #'(getter-id ...))
                         (syntax-violation
                          copier-name "unknown getter" x id)))
                   #'(getter ...))
         (with-syntax ((unsafe-expr
                        #`(let ((new (allocate-struct type-name #,nfields)))
                            #,@(map (lambda (getter index)
                                      #`(struct-set!
                                         new
                                         #,index
                                         #,(lookup getter
                                                   #`(struct-ref s #,index))))
                                    #'(getter-id ...)
                                    (iota nfields))
                            new)))
           (if (syntax->datum #'check?)
               #`(if (eq? (struct-vtable s) type-name)
                     unsafe-expr
                     (throw-bad-struct
                      s '#,(datum->syntax #'here copier-name)))
               #'unsafe-expr)))))))

(define-syntax %define-record-type
  (lambda (x)
    (define (field-identifiers field-specs)
      (map (lambda (field-spec)
             (syntax-case field-spec ()
               ((name getter) #'name)
               ((name getter setter) #'name)))
           field-specs))

    (define (getter-identifiers field-specs)
      (map (lambda (field-spec)
             (syntax-case field-spec ()
               ((name getter) #'getter)
               ((name getter setter) #'getter)))
           field-specs))

    (define (constructor form type-name constructor-spec field-ids)
      (syntax-case constructor-spec ()
        ((ctor field ...)
         (every identifier? #'(field ...))
         (let ((slots (map (lambda (field)
                             (or (list-index (lambda (x)
                                               (free-identifier=? x field))
                                             field-ids)
                                 (syntax-violation
                                  (syntax-case form ()
                                    ((macro . args)
                                     (syntax->datum #'macro)))
                                  "unknown field in constructor spec"
                                  form field)))
                           #'(field ...))))
           #`(define-inlinable #,constructor-spec
               (let ((s (allocate-struct #,type-name #,(length field-ids))))
                 #,@(map (lambda (arg slot)
                           #`(struct-set! s #,slot #,arg))
                         #'(field ...) slots)
                 s))))))

    (define (getters type-name getter-ids copier-id)
      (map (lambda (getter index)
             #`(define-tagged-inlinable
                 ((%%type   #,type-name)
                  (%%index  #,index)
                  (%%copier #,copier-id))
                 (#,getter s)
                 (if (eq? (struct-vtable s) #,type-name)
                     (struct-ref s #,index)
                     (throw-bad-struct s '#,getter))))
           getter-ids
           (iota (length getter-ids))))

    (define (copier type-name getter-ids copier-id)
      #`(define-syntax-rule
          (#,copier-id check? s (getter expr) (... ...))
          (%%set-fields #,type-name #,getter-ids
                        check? s (getter expr) (... ...))))

    (define (setters type-name field-specs)
      (filter-map (lambda (field-spec index)
                    (syntax-case field-spec ()
                      ((name getter) #f)
                      ((name getter setter)
                       #`(define-inlinable (setter s val)
                           (if (eq? (struct-vtable s) #,type-name)
                               (struct-set! s #,index val)
                               (throw-bad-struct s 'setter))))))
                  field-specs
                  (iota (length field-specs))))

    (define (functional-setters copier-id field-specs)
      (filter-map (lambda (field-spec index)
                    (syntax-case field-spec ()
                      ((name getter) #f)
                      ((name getter setter)
                       #`(define-inlinable (setter s val)
                           (#,copier-id #t s (getter val))))))
                  field-specs
                  (iota (length field-specs))))

    (define (record-layout immutable? count)
      ;; Mutability is expressed on the record level; all structs in the
      ;; future will be mutable.
      (string-concatenate (make-list count "pw")))

    (syntax-case x ()
      ((_ immutable? form type-name constructor-spec predicate-name
          field-spec ...)
       (let ()
         (define (syntax-error message subform)
           (syntax-violation (syntax-case #'form ()
                               ((macro . args) (syntax->datum #'macro)))
                             message #'form subform))
         (and (boolean? (syntax->datum #'immutable?))
              (or (identifier? #'type-name)
                  (syntax-error "expected type name" #'type-name))
              (syntax-case #'constructor-spec ()
                ((ctor args ...)
                 (every identifier? #'(ctor args ...))
                 #t)
                (_ (syntax-error "invalid constructor spec"
                                 #'constructor-spec)))
              (or (identifier? #'predicate-name)
                  (syntax-error "expected predicate name" #'predicate-name))
              (every (lambda (spec)
                       (syntax-case spec ()
                         ((field getter) #t)
                         ((field getter setter) #t)
                         (_ (syntax-error "invalid field spec" spec))))
                     #'(field-spec ...))))
       (let* ((field-ids   (field-identifiers  #'(field-spec ...)))
              (getter-ids  (getter-identifiers #'(field-spec ...)))
              (field-count (length field-ids))
              (immutable?  (syntax->datum #'immutable?))
              (layout      (record-layout immutable? field-count))
              (ctor-name   (syntax-case #'constructor-spec ()
                             ((ctor args ...) #'ctor)))
              (copier-id   (make-copier-id #'type-name)))
         #`(begin
             #,(constructor #'form #'type-name #'constructor-spec field-ids)

             (define type-name
               (let ((rtd (make-struct/no-tail
                           record-type-vtable
                           '#,(datum->syntax #'here (make-struct-layout layout))
                           default-record-printer
                           'type-name
                           '#,field-ids)))
                 (set-struct-vtable-name! rtd 'type-name)
                 (struct-set! rtd (+ 2 vtable-offset-user) #,ctor-name)
                 rtd))

             (define-inlinable (predicate-name obj)
               (and (struct? obj)
                    (eq? (struct-vtable obj) type-name)))

             #,@(getters #'type-name getter-ids copier-id)
             #,(copier #'type-name getter-ids copier-id)
             #,@(if immutable?
                    (functional-setters copier-id #'(field-spec ...))
                    (setters #'type-name #'(field-spec ...))))))
      ((_ immutable? form . rest)
       (syntax-violation
        (syntax-case #'form ()
          ((macro . args) (syntax->datum #'macro)))
        "invalid record definition syntax"
        #'form)))))

(define-syntax-rule (define-record-type name ctor pred fields ...)
  (%define-record-type #f (define-record-type name ctor pred fields ...)
                       name ctor pred fields ...))
