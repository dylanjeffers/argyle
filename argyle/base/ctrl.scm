(ns (argyle base ctrl)
    :replace (do = aif it & $> nil?))
(use ((srfi srfi-1) :select (append-map lset-difference))
     (argyle guile)
     (argyle base mac)
     (argyle base fn)
     (ice-9 control))
(re-export abort (call/cc . c/cc) (call/ec . c/ec))

(mac do ((e1 ...) #'(begin e1 ...)))

;;; TODO: check if var is a free variable, and if so, define it
(mac =
  ((var val) #'(set! var val))
  ((var val rest ...) #'(do (set! var val) (= rest ...))))


(mac aif x
  ((test then else)
   (let-syn it (datum->syntax #'then 'it)
     #'(let it test (if it then else)))))

(mac & ((e1 ...) #'(and e1 ...)))

(defp c/prmt call-with-prompt)
(defp c/vals call-with-values)
(defp tag default-prompt-tag)
(defp vals values)
(defp =? _=)
(defp 0? zero?)
(defp 1? (n) (=? 1 n))
(defp ~ not)
(defp flatn append-map)
(defp &map and-map)
(defp set\ lset-difference)
(def nil? null?)

(mac $>
  ((exp)           #'(c/prmt (tag) (fn () exp) hdlr))
  ((exp hdlr)      #'(c/prmt (tag) (fn () exp) hdlr))
  ((tag expr hdlr) #'(c/prmt (tag) (fn () exp) hdlr)))

(def hdlr (cont f)
  ($> (tag) (f cont) hdlr))
