;;; Minimal test harness shared by the suites in this directory.
;;;
;;;   (check name expected exp)   pass when exp is equal? to expected
;;;   (check-err name exp)        pass when exp raises
;;;   (check-out name str exp)    pass when exp prints str to stdout
;;;   (done)                      print totals; exit 1 if anything failed

(ns (test check)
    :export (check check-err check-out done))
(use (argyle))

(def passed 0)
(def failed 0)

(def record! (name ok? expected got)
  (if ok?
      (=! passed (1+ passed))
      (do (=! failed (1+ failed))
          (format "FAIL ~a\n  expected: ~s\n  got:      ~s\n" name expected got))))

(def raised (key . args)
  `(raised ,key ,@args))

(mac check
  ((name expected exp)
   #'(let got (catch #t (fn () exp) raised)
       (record! name (equal? got expected) expected got))))

(mac check-err
  ((name exp)
   #'(let got (catch #t (fn () exp (list 'returned)) (fn _ 'raised))
       (record! name (eq? got 'raised) 'raised got))))

(mac check-out
  ((name expected exp)
   #'(check name expected (with-output-to-string (fn () exp)))))

(def done ()
  (format "~a passed, ~a failed\n" passed failed)
  (exit (if (0? failed) 0 1)))
