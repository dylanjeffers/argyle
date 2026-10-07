(ns (argyle generic)
  :export (gen <gen-fn> gen-fn? xtnd type
           str len rev join cpy clr! kth
           map))
(use (argyle base)
     ((argyle base type)
      :select ((str . _str)))
     (argyle data)
     (argyle data tbl)
     (argyle data vec)
     (argyle data q)
     (argyle guile)
     (argyle loop)
     (srfi srfi-1))

(mac gen
  ((name f) (id? #'name)
   #'(def name (%gen-fn 'name (tbl 'def f))))
  ((name) (id? #'name)
   #'(def name (%gen-fn 'name (let f (imported-ref (current-module) 'name)
                                (if f (tbl 'def f) (mke-tbl)))))))

;;; Compiled modules declare their own top-level vars before running, so
;;; (defined? 'car) would see the module's unbound car; look in imports.
(def imported-ref (mod name)
  (let v (or-map (fn (m) (module-variable m name)) (module-uses mod))
    (and v (variable-bound? v) (variable-ref v))))

(trans gen-fn (name tbl)
  :init (%gen-fn name tbl)
  :app (fn args
         (apply (resolve-fn (gen-fn-tbl self) args)
                args)))

;;; This version works, but needs cleanup
(def resolve-fn (tbl args)
  (loop lp ((for arg (in-list args))
            (where t tbl (and=> t (\\ _ (type arg)))))
        => (cond ((and t (t 'fn)) (t 'fn))
                 ((and t (t 'rst)) (t 'rst))
                 ((tbl 'def) (tbl 'def))
                 (else (err "No generic fn for args1:" args)))
    ;; This handles . rest case
    (if t
        (aif (t 'rst) it (lp))
        (aif (tbl 'def) it 
            (err "No generic fn for args:" args)))))

(def type (x)
  (if (data? x) (data-type x)
      (base-type x)))

;;; Going to straight cpy for this version
(mac xtnd x
  (def split (lst)
    (c/vals (fn () (unzip2 (grp lst 2))) list))
  ((fn-name (arg1 ... . rest) body ...) (~(nil? #'rest))
   (let-syn (args types) (split #'(arg1 ...))
     #`(loop ((for type  (in-list 'types))
              (where type-tree (gen-fn-tbl fn-name)
                (or (type-tree type)
                    (do (type-tree type (mke-tbl))
                        (type-tree type)))))
        => (type-tree 'rst (fn (#,@#'args . rest) body ...)))))
  ((fn-name (arg1 ...) body ...)
   (let-syn (args types) (split #'(arg1 ...))
            ;; TODO: refactor
     #`(loop ((for type (in-list 'types))
              (where type-tree (gen-fn-tbl fn-name)
                     (or (type-tree type)
                         (do (type-tree type (mke-tbl))
                             (type-tree type)))))
         => (type-tree 'fn (fn args body ...))))))

(gen len length)
(gen rev reverse)
(gen join append)
(gen cpy lst-cpy)
(gen clr! (fn (lst) (set-cdr! lst '())))

(gen car)
(gen cdr)
(gen kth list-ref)
(gen take)
(gen drop)

(defp str args
  (reduce-right str-join "" (map _str args)))

(xtnd len (s <str>) (str-len s))
(xtnd len (n <int>) (str-len (str n)))
(xtnd len (t <tbl>) (tbl-cnt (const #t) t))
(xtnd len (v <vec>) (vec-len v))
(xtnd len (q <q>) (q-len q))
(xtnd len (stream <strm>) (strm-len stream))

(xtnd rev (s <str>) (string-reverse s))

(xtnd join (s1 <str> . rest) (apply str-join s1 rest))

(xtnd join (strms <strm>) (strm-join strms))

(xtnd cpy (v <vec>) (vec-cpy v))
(xtnd cpy (q <q>) (%mke-q (q-len q) (q-hd q) (q-tl q)))
(xtnd clr! (t <tbl>) (tbl-clr! t))
(xtnd clr! (q <q>) (q-hd! q '()) (q-tl! q '()) (q-len! q 0))

(xtnd car (seq <strm>) (scar seq))
(xtnd car (seq <vec>) (seq 0))
(xtnd car (seq <q>) (q-pk seq))
(xtnd cdr (seq <strm>) (scdr seq))
(xtnd take (seq <strm> k <int>) (strm-take k seq))
(xtnd drop (seq <strm> k <int>) (strm-drop k seq))
(xtnd kth (seq <vec> k <int>) (seq k))

;;; Collections

(gen map (@ (srfi srfi-1) map))
(xtnd map (f <fn> v <vec>) (vec-map f v))
(xtnd map (f <fn> s <str> . rst) (apply str-map f s rst))
(xtnd map (f <fn> t <tbl>) (tbl-map->lst f t))
