(ns (argyle data set)
    :export (<set> set?))
(use (argyle base)
     (argyle data))

;;; Sets are applicable: (s x) adds x and returns s.
(data! set (hash)
   :init (%make-set hash)
   :app (fn (v) (hash-set! (set-hash self) v v) self))

(defp set args
  (ret set (%make-set (make-hash-table))
    (for-each (\\ set _) args)))

(defp has? (set v)
  (if (hash-get-handle (set-hash set) v) #t #f))

(defp elements (set)
  (hash-map->list (fn (k v) v) (set-hash set)))

(defp set-clear! (set)
  (hash-clear! (set-hash set)))
