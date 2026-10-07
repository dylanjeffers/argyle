(ns (argyle data queue)
  :export (<queue> queue? queue-length))
(use (argyle base)
     (argyle data)
     (argyle data vec))

;;; Queues are applicable: (q x) enqueues x, and (q) dequeues.
;;; Items are enqueued onto tail and dequeued from head; tail is reversed
;;; into head when head runs out.
(data! queue (length head tail)
  :init (%make-queue length head tail)
  :app (fns
        (() (dequeue! self))
        ((k) (enqueue! self k))))

(defp make-queue () (%make-queue 0 '() '()))
(defp queue args (%make-queue (length args) args '()))
(defp queue-empty? (q) (0? (queue-length q)))
(defp queue-peek (q) (if (nil? (queue-head q))
                         (if (nil? (queue-tail q)) #f
                             (car (queue-tail q)))
                         (car (queue-head q))))

(defp enqueue! (q obj)
  (queue-tail! q (cons obj (queue-tail q)))
  (queue-length! q (1+ (queue-length q))))

(defp dequeue! (q)
  (if (queue-empty? q) (error "Can't dequeue an empty queue!")
      (%dequeue! q)))

(def %dequeue! (q)
  (when (nil? (queue-head q)) (move-tail->head! q))
  (ret val (car (queue-head q))
       (queue-length! q (1- (queue-length q)))
       (queue-head! q (cdr (queue-head q)))))

(def move-tail->head! (q)
  (queue-head! q (reverse (queue-tail q)))
  (queue-tail! q '()))

(defp queue-clear! (q)
  (queue-head! q '())
  (queue-tail! q '())
  (queue-length! q 0))

(defp queue->list (q)
  (append (queue-head q) (reverse (queue-tail q))))

(defp list->queue (lst)
  (%make-queue (length lst) lst '()))

(defp queue->vec (compose list->vec queue->list))
(defp vec->queue (compose list->queue vec->list))
