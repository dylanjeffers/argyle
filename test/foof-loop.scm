;;; Runs the vendored foof-loop test suite (argyle/loop/test-foof-loop.scm)
;;; against (argyle loop), with just enough of a test harness for it.
;;; Run with: test/run foof-loop

(use-modules (argyle loop)
             (rnrs io ports))

(define passed 0)
(define failed 0)

(define (report-failure where msg . irritants)
  (set! failed (1+ failed))
  (format #t "FAIL ~a: ~a ~s\n" where msg irritants))

(define current-case #f)

(define (check same? expr expected got)
  (if (same? expected got)
      (set! passed (1+ passed))
      (report-failure current-case expr 'expected expected 'got got)))

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ . rest) (if #f #f))))

(define cases '())

;; Cases are registered and run later by RUN-TEST-SUITE, since some of
;; them call helpers defined further down the file.
(define-syntax define-test-case
  (syntax-rules ()
    ((_ suite name () body ...)
     (set! cases (cons (cons (symbol-append 'suite '/ 'name)
                             (lambda () body ...))
                       cases)))))

(define-syntax-rule (run-test-suite . _) (run-cases))

(define (run-cases)
  (for-each (lambda (c)
              (set! current-case (car c))
              (catch #t
                (cdr c)
                (lambda (key . args)
                  (report-failure (car c) key args))))
            (reverse cases)))

(define-syntax test-equal
  (syntax-rules ()
    ((_ expected expr) (check equal? 'expr expected expr))))

(define-syntax test-eqv
  (syntax-rules ()
    ((_ expected expr) (check eqv? 'expr expected expr))))

(define (test-failure msg . irritants)
  (apply report-failure current-case msg irritants))

;; Argyle renamed foof-loop's WITH clause to WHERE; the vendored tests
;; still use the upstream name, so rename it while reading them in.
(define (rename-with x)
  (cond ((eq? x 'with) 'where)
        ((pair? x) (cons (rename-with (car x)) (rename-with (cdr x))))
        (else x)))

(call-with-input-file
    (string-append (dirname (current-filename))
                   "/../argyle/loop/test-foof-loop.scm")
  (lambda (port)
    (let lp ((form (read port)))
      (unless (eof-object? form)
        (primitive-eval (rename-with form))
        (lp (read port))))))

(format #t "~a passed, ~a failed\n" passed failed)
(exit (if (zero? failed) 0 1))
