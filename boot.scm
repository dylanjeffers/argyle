;;; boot.scm -- bootstrap the Argyle module syntax into Guile.
;;;
;;; Argyle source files open with (ns ...) and (use ...) instead of
;;; define-module / use-modules, and write keywords Arc-style (:export).
;;; Fresh module environments only see the (guile) root module, so the
;;; syntax has to live there before any argyle module is loaded.

(read-set! keywords 'prefix)

;;; Argyle replaces core bindings (let, map, +, format, ...) on purpose.
;;; Guile's default duplicate handling warns every time one is first used,
;;; so register a quiet variant of its warn-override-core handler: prefer
;;; the non-core binding without a warning.  Clashes between two non-core
;;; modules still warn.
(module-define! duplicate-handlers 'override-core
  (lambda (module name int1 val1 int2 val2 var val)
    (and (eq? int1 the-scm-module)
         (module-variable int2 name))))

(define %argyle-duplicates '(replace override-core warn last))

;;; Scripts and the REPL run in (guile-user).
(default-duplicate-binding-handler %argyle-duplicates)

(eval
 '(begin
    (define-syntax ns
      (lambda (x)
        (syntax-case x ()
          ((_ name opt ...)
           (if (memq #:duplicates (syntax->datum #'(opt ...)))
               #'(define-module name opt ...)
               #'(define-module name opt ...
                   #:duplicates (replace override-core warn last)))))))
    (define-syntax use
      (syntax-rules ()
        ((_ spec ...) (use-modules spec ...))))
    (export-syntax ns use))
 the-root-module)
