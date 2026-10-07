;;; (argyle fibers); needs guile-fibers (brew install guile-fibers).
;;; Run with: test/run fibers

(use (argyle) (argyle fibers) (fibers channels) (fibers timers) (test check))

(check "go returns after all fibers finish" '(a b)
       (let out '()
         (go (=! out (cons 'a out))
             (=! out (cons 'b out)))
         (sort out (fn (x y) (string<? (symbol->string x) (symbol->string y))))))

(check "channels pass values between fibers" 42
       (let ch (make-channel)
         (let got #f
           (go (put-message ch (* 6 7))
               (=! got (get-message ch)))
           got)))

(check "fibers interleave on sleep" '(first second)
       (let out '()
         (go (do (sleep 0.05) (=! out (cons 'second out)))
             (=! out (cons 'first out)))
         (rev out)))

(done)
