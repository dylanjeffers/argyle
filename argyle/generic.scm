;;; Generic functions: generic defines one, extend adds a method for a list of
;;; argument types.  The generics themselves live in (argyle generic base)
;;; and (argyle generic coll).
(ns (argyle generic)
  :export (generic <generic> generic? extend type))
(use (argyle base)
     (argyle data)
     (argyle data table)
     (argyle guile)
     (argyle loop)
     ((srfi srfi-1) :select (unzip2)))

(mac generic
  ((name f) (id? #'name)
   #'(def name (%make-generic 'name (table 'def f))))
  ((name) (id? #'name)
   #'(def name (%make-generic 'name (let f (imported-ref (current-module) 'name)
                                (if f (table 'def f) (make-table)))))))

;;; Compiled modules declare their own top-level vars before running, so
;;; (defined? 'car) would see the module's unbound car; look in imports.
(def imported-ref (mod name)
  (let v (or-map (fn (m) (module-variable m name)) (module-uses mod))
    (and v (variable-bound? v) (variable-ref v))))

(data! generic (name table)
  :init (%make-generic name table)
  :app (fn args
         (apply (resolve-fn (generic-table self) args)
                args)))

;;; Methods live in a tree of tbls keyed by each argument's type in turn.
;;; An argument follows its exact type's branch, else <fn> if it's
;;; applicable (so generics, vecs, tbls... count as functions), else <any>.
(def branch (t arg)
  (or (t (type arg))
      (and (procedure? arg) (t '<fn>))
      (t '<any>)))

;;; This version works, but needs cleanup
(def resolve-fn (table args)
  (loop lp ((for arg (in-list args))
            (where t table (and=> t (\\ branch _ arg))))
        => (cond ((and t (t 'fn)) (t 'fn))
                 ((and t (t 'rst)) (t 'rst))
                 ((table 'def) (table 'def))
                 (else (error "No generic fn for args1:" args)))
    ;; This handles . rest case
    (if t
        (aif (t 'rst) it (lp))
        (aif (table 'def) it
            (error "No generic fn for args:" args)))))

(def type (x)
  (if (data? x) (data-type x)
      (base-type x)))

;;; Going to straight copy for this version
(mac extend x
  (def split (lst)
    (call-with-values (fn () (unzip2 (grp lst 2))) list))
  ((procedure-name (arg1 ... . rest) body ...) (~(nil? #'rest))
   (let-syn (args types) (split #'(arg1 ...))
     #`(loop ((for type  (in-list 'types))
              (where type-tree (generic-table procedure-name)
                (or (type-tree type)
                    (do (type-tree type (make-table))
                        (type-tree type)))))
        => (type-tree 'rst (fn (#,@#'args . rest) body ...)))))
  ((procedure-name (arg1 ...) body ...)
   (let-syn (args types) (split #'(arg1 ...))
            ;; TODO: refactor
     #`(loop ((for type (in-list 'types))
              (where type-tree (generic-table procedure-name)
                     (or (type-tree type)
                         (do (type-tree type (make-table))
                             (type-tree type)))))
         => (type-tree 'fn (fn args body ...))))))
