;;; Generic versions of base functions.
(ns (argyle generic base)
  :export (str len rev join kth))
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

(gen len length)
(xtnd len (s <str>) (string-length s))
(xtnd len (n <int>) (string-length (str n)))
(xtnd len (t <table>) (table-count (const #t) t))
(xtnd len (v <vec>) (vec-length v))
(xtnd len (q <queue>) (queue-length q))
(xtnd len (s <set>) (length (elements s)))
(xtnd len (stream <stream>) (stream-length stream))

(gen rev reverse)
(xtnd rev (s <str>) (string-reverse s))

(gen join append)
(xtnd join (s1 <str> . rest) (apply string-append s1 rest))
(xtnd join (strms <stream>) (stream-concat strms))
(xtnd join (s1 <stream> . rest) (apply stream-append s1 rest))

(gen kth list-ref)
(xtnd kth (seq <vec> k <int>) (seq k))

;;; Not exported yet: exporting them would replace the core car / cdr /
;;; take / drop everywhere argyle is used.
(gen car)
(gen cdr)
(gen take)
(gen drop)
(xtnd car (seq <stream>) (stream-car seq))
(xtnd car (seq <vec>) (seq 0))
(xtnd car (seq <queue>) (queue-peek seq))
(xtnd cdr (seq <stream>) (stream-cdr seq))
(xtnd take (seq <stream> k <int>) (stream-take k seq))
(xtnd drop (seq <stream> k <int>) (stream-drop k seq))
