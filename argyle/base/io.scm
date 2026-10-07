(ns (argyle base io)
  :export (print println print-lines)
  :replace (format))
(use (argyle base mac)
     (argyle base fn)
     (argyle base ctrl)
     (ice-9 pretty-print))
(re-export pretty-print)

(mac print
  ((v1) #'(display v1))
  ((v1 v2 ...) #'(do (display v1) (print v2 ...))))

(mac println
  ((v1 v2 ...) #'(do (print v1 v2 ...) (newline))))

(mac print-lines
  ((v1) #'(println v1))
  ((v1 v2 ...) #'(do (println v1) (print-lines v2 ...))))


(defp format (str . args)
  (apply (@ (guile) format) #t str args))
