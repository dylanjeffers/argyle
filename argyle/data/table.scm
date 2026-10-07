(ns (argyle data table)
  :export (<table> table? table-hash))
(use (argyle base)
     (argyle loop)
     ((argyle guile) :select (grp))
     (argyle data))

;;; Tables are applicable: (t) is the underlying hash table, (t k) looks k
;;; up, and (t k v) sets it and returns t.
;;; TODO: allow init size and comparison operators
(data! table (hash)
  :init (%make-table hash)
  :app (fns
        (() (table-hash self))
        ((k) (hash-ref (table-hash self) k))
        ((k v)
         (hash-set! (table-hash self) k v)
         self)))

(defp make-table (#:o (n 0))
  (%make-table (make-hash-table n)))
(defp table args
  (ret t (make-table)
       (for-each (\\ apply t _) (grp args 2))))

(defp table-delete! (t k) (hash-remove! (t) k))
(defp table-count (pred t) (hash-count pred (t)))
(defp table-clear! (t) (hash-clear! (t)))
(defp table-fold (f init t) (hash-fold f init (t)))
(defp table-for-each (f t) (hash-for-each f (t)))
(defp table-map->list (f t) (hash-map->list f (t)))

(defp update (t k fn)
  (t k (fn (t k))))

(def ifcons (head tail)
  (if tail (cons head tail)
      (list head)))

(defp group-by (pred seq)
  (loop ((for elt (in-list seq))
         (where t (table)
                (update t (pred elt)
                        (\\ ifcons elt _))))
    => (table-map->list (fn (k v) (list k (reverse v)))
                        t)))
