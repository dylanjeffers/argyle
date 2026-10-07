(ns (argyle data vec)
  :export (<vec> vec?))
(use (argyle base)
     (argyle data)
     (srfi srfi-43))

;;; Vecs are applicable: (v) is the underlying vector, (v k) reads index k,
;;; and (v k x) writes it.
;;; TODO: add optional fill
(data! vec (v)
  :init (%make-vec v)
  :app (let v (vec-v self)
         (fns
          (() v)
          ((k) (vector-ref v k))
          ((k obj) (vector-set! v k obj)))))

(defp make-vec (len #:o fill)
  (%make-vec (make-vector len fill)))
(defp vec args (list->vec args))
(defp vec-length (v) (vector-length (v)))
(defp vec->list (v) (vector->list (v)))
(defp list->vec (lst) (%make-vec (list->vector lst)))
(defp vec-copy (v) (%make-vec (vector-copy (v))))
(defp vec-fill! (v fill) (vector-fill! (v) fill))
(defp vec-map (f v . vs)
  (%make-vec
   (apply vector-map (fn (i e1 . es) (apply f e1 es))
          (v)
          (map (fn (v) (v)) vs))))
;;; Etc...
