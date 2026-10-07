(ns (argyle data vec)
  :export (<vec> vec vec? vec-fn vec-fn!))
(use (argyle base)
     (argyle data)
     (srfi srfi-43))

;;; TODO: add optional fill
(data! vec (v)
  :init (%mke-vec v)
  :app (let v (vec-v self)
         (fns
          (() v)
          ((k) (vector-ref v k))
          ((k obj) (vector-set! v k obj)))))

(defp mke-vec (len #:o fill)
  (%mke-vec (make-vector len fill)))
(defp vec args (lst->vec args))
(defp vec: (v k) (v k))
(defp vec! (v k obj) (v k obj))
(defp vec-len (v) (vector-length (v)))
(defp vec->lst (v) (vector->list (v)))
(defp lst->vec (lst) (%mke-vec (list->vector lst)))
(defp vec-cpy (v) (%mke-vec (vector-copy (v))))
(defp vec-fill! (v fill) (vector-fill! (v) fill))
(defp vec-map (f v . vs)
  (%mke-vec
   (apply vector-map (fn (i e1 . es) (apply f e1 es))
          (v)
          (map (fn (v) (v)) vs))))
;;; Etc...
