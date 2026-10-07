(ns (argyle conc)
  #:replace (@)
  :export (<ref> ref ref? doasync dosync alter))
(use (argyle base)
     (argyle guile)
     (argyle loop)
     (argyle data)
     ((srfi srfi-1) :select (zip))
     (ice-9 futures)
     (ice-9 threads))
(re-export future future? make-future touch)

(data ref (val mutx)
  :init (%ref val mutx))

(defp ref (val) (%ref val (make-mutex)))

;;; Deref a future (waiting for it) or a ref.
(defp @ (obj)
    (cond ((future? obj) (touch obj))
          ((ref? obj) (ref-val obj))
          (else (error "attempt to deref a non-lazy obj" obj))))

(mac doasync
  ((e0 ...) #'(parallel e0 ...)))

(mac dosync x
  (((refs ...) body ...) 
   (let ref-dats (map (fn (ref) (syn->dat ref)) #'(refs ...))
     (w/syn ((ref-cpys ...) (map (fn (ref) (datum->syntax x ref)) ref-dats)
             alter (syn 'alter x))
       #`(w/refs (refs ...)
           (let refs* (_let #,(zip #'(ref-cpys ...)
                                   (map (fn (ref) #`(cpy-ref #,ref))
                                        #'(refs ...)))
                            body ...
                            (list ref-cpys ...))
             (set-refs! (refs ...) refs*)))))))

;;; TODO: only allow user to use alter in a dosync block
(mac alter
  ((ref val) 
   #'(with-mutex (ref-mutx ref)
       (set! ref (ref-val! ref val)))))

(mac set-refs!
  (((refs ...) values)
   #`(do #,@(map (fn (ref val) #`(set! #,ref (ref-val! #,ref (ref-val #,val))))
                 #'(refs ...)
                 #`(#,@(map (fn (i) #`(list-ref values #,i))
                            (iota (_length #'(refs ...)))))))))

(mac w/refs
  (((r1 ...) e1 ...)
   #`(w/mutxs #,(map (fn (ref) #`(ref-mutx #,ref))
                     #'(r1 ...))
              e1 ...)))

(mac w/mutxs
  (((m1 ...) e1 ...)
   #`(#,@(loop lp ((ms #'(m1 ...)))
           (if (nil? ms) #'(do e1 ...)
               #`(with-mutex #,(car ms)
                   #,(lp (cdr ms))))))))


(eval-when (expand load eval compile)
  (def cpy-ref (ref) (ref-mutx! ref (make-mutex))))
