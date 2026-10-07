;;; boot.scm -- bootstrap the Argyle module syntax into Guile.
;;;
;;; Argyle source files open with (ns ...) and (use ...) instead of
;;; define-module / use-modules, and write keywords Arc-style (:export).
;;; Fresh module environments only see the (guile) root module, so the
;;; syntax has to live there before any argyle module is loaded.

(read-set! keywords 'prefix)

(eval
 '(begin
    (define-syntax ns
      (syntax-rules ()
        ((_ name opt ...) (define-module name opt ...))))
    (define-syntax use
      (syntax-rules ()
        ((_ spec ...) (use-modules spec ...))))
    (export-syntax ns use))
 the-root-module)
