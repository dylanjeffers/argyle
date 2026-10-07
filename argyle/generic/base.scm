;;; Generic versions of base functions.
(ns (argyle generic base)
  :export (str length reverse join nth first rest take drop))
(use (argyle base)
     ((argyle base type) :select ((str . _str)))
     (argyle generic)
     (argyle data table)
     (argyle data vec)
     (argyle data queue)
     (argyle data set)
     ((argyle lib streams) :select (stream-append))
     ((srfi srfi-1) :select (reduce-right)))

(defp str args
  (reduce-right string-append "" (map _str args)))

(generic length)
(extend length (v <vector>) (vector-length v))
(extend length (h <hash-table>) (hash-count (const #t) h))
(extend length (s <str>) (string-length s))
(extend length (n <int>) (string-length (str n)))
(extend length (t <table>) (table-count (const #t) t))
(extend length (v <vec>) (vec-length v))
(extend length (q <queue>) (queue-length q))
(extend length (s <set>) (length (elements s)))
(extend length (stream <stream>) (stream-length stream))

(generic reverse)
(extend reverse (s <str>) (string-reverse s))

(generic join append)
(extend join (s1 <str> . rest) (apply string-append s1 rest))
(extend join (strms <stream>) (stream-concat strms))
(extend join (s1 <stream> . rest) (apply stream-append s1 rest))

(generic nth list-ref)
(extend nth (seq <vec> k <int>) (seq k))
(extend nth (seq <stream> k <int>) (stream-ref seq k))

;;; first / rest are generic car / cdr; car and cdr themselves stay as
;;; Scheme's.
(generic first car)
(generic rest cdr)
(generic take (@ (srfi srfi-1) take))
(generic drop (@ (srfi srfi-1) drop))
(extend first (seq <stream>) (stream-car seq))
(extend first (seq <vec>) (seq 0))
(extend first (seq <queue>) (queue-peek seq))
(extend rest (seq <stream>) (stream-cdr seq))
(extend take (seq <stream> k <int>) (stream-take k seq))
(extend drop (seq <stream> k <int>) (stream-drop k seq))
