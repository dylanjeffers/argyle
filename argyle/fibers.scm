(ns (argyle fibers)
    :export (go))
(use (fibers)
     (argyle base))

;;; Run each expression in its own fiber; returns once all have finished.
(mac go
  ((exp ...)
   #'(run-fibers
      (fn ()
        (spawn-fiber (fn () exp)) ...)
      :drain? #t
      ;; No preemption: guile-fibers' macOS build calls an undefined
      ;; pthread-kill when stopping its preemption timer.  Fibers still
      ;; yield on channel ops and sleeps.
      :hz 0)))
