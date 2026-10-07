(ns (argyle data q)
  :export (<q> q mke-q %mke-q q? q-nil?
            q-pk enq! deq!
            q-hd q-hd! q-tl q-tl! q-len q-len! q-fn q-fn!
            q->lst lst->q q->vec vec->q))
(use (argyle base)
     (argyle data)
     (argyle data vec))

(data! q (len hd tl)
  :init (%mke-q len hd tl)
  :app (fns
        (() (deq! self))
        ((k) (enq! self k))))

(defp mke-q () (%mke-q 0 '() '()))
(defp q args (%mke-q (length args) args '()))
(defp q-nil? (q) (0? (q-len q)))
(defp q-pk (q) (if (nil? (q-hd q))
                   (if (nil? (q-tl q)) #f
                       (car (q-tl q)))
                   (car (q-hd q))))

(defp enq! (q obj)
  (q-tl! q (cons obj (q-tl q)))
  (q-len! q (1+ (q-len q))))

(defp deq! (q)
  (if (q-nil? q) (error "Can't dequeue an empty queue!")
      (%deq! q)))

(def %deq! (q)
  (when (nil? (q-hd q)) (move-tl->hd! q))
  (ret val (car (q-hd q))
       (q-len! q (1- (q-len q)))
       (q-hd! q (cdr (q-hd q)))))

(def move-tl->hd! (q)
  (q-hd! q (reverse (q-tl q)))
  (q-tl! q '()))

(defp q->lst (q)
  (append (q-hd q) (reverse (q-tl q))))

(defp lst->q (lst)
  (%mke-q (length lst) lst '()))

(defp q->vec (compose lst->vec q->lst))
(defp vec->q (compose lst->q vec->lst))
