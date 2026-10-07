;;; Generic versions of base functions.
(ns (argyle generic base)
  :export (str len rev join kth))
(use (argyle base)
     ((argyle base type) :select ((str . _str)))
     (argyle generic)
     (argyle data tbl)
     (argyle data vec)
     (argyle data q)
     (argyle data set)
     ((argyle lib streams) :select (stream-append))
     ((srfi srfi-1) :select (reduce-right)))

(defp str args
  (reduce-right string-append "" (map _str args)))

(gen len length)
(xtnd len (s <str>) (string-length s))
(xtnd len (n <int>) (string-length (str n)))
(xtnd len (t <tbl>) (tbl-cnt (const #t) t))
(xtnd len (v <vec>) (vec-len v))
(xtnd len (q <q>) (q-len q))
(xtnd len (s <set>) (length (elements s)))
(xtnd len (stream <strm>) (strm-len stream))

(gen rev reverse)
(xtnd rev (s <str>) (string-reverse s))

(gen join append)
(xtnd join (s1 <str> . rest) (apply string-append s1 rest))
(xtnd join (strms <strm>) (strm-join strms))
(xtnd join (s1 <strm> . rest) (apply stream-append s1 rest))

(gen kth list-ref)
(xtnd kth (seq <vec> k <int>) (seq k))

;;; Not exported yet: exporting them would replace the core car / cdr /
;;; take / drop everywhere argyle is used.
(gen car)
(gen cdr)
(gen take)
(gen drop)
(xtnd car (seq <strm>) (scar seq))
(xtnd car (seq <vec>) (seq 0))
(xtnd car (seq <q>) (q-pk seq))
(xtnd cdr (seq <strm>) (scdr seq))
(xtnd take (seq <strm> k <int>) (strm-take k seq))
(xtnd drop (seq <strm> k <int>) (strm-drop k seq))
