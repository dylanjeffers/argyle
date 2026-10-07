(ns (argyle match))
(export-syntax w/ let def fn fn-case)
(use (argyle base)
     ((argyle base fn)
      :select ((def . _def)
               (let . _let)
               (w/ . _w/)
               (fn . _fn)
               (fns . _fns)))
     ((argyle data tbl) :select (<tbl>))
     (argyle loop)
     (argyle match compat)
     ((srfi srfi-1) :select (zip reduce append-map)))

(mac fn
  ((args b1 b2 ...)
   #`(_fn #,@(fn-match #'(args b1 b2 ...)))))

(mac fn-case
  ((((pat ... . rst) b1 b2 ...) ...)
   #`(_fns
      #,@(map fn-match
              #'(((pat ... . rst) b1 b2 ...) ...)))))

(mac def
  ((name exp)
   #'(_def name exp))
  ((name (pat ... . rst) b1 b2 ...)
   #`(_def name #,@(fn-match #'((pat ... . rst) b1 b2 ...)))))

(mac let
  ((pat exp . bdy) #'(w/ (pat exp) . bdy)))

(mac w/
  ((pat:exp . bdy) #'(match-xpnd pat:exp . bdy)))

;;; TODO: modularize w/ def
(mac match-xpnd (:keys)
  ((() . bdy)
   #'(do . bdy))
  ((((:keys key ...) tbl . rst) . bdy)
   #'(tbl-match tbl ((:keys key ...)
                     (match-xpnd rst . bdy))))
  (((kwd kwd' . rst) . bdy) (kwd? (syn->dat #'kwd))
   #'(op-match-xpnd rst . bdy))
  (((pat exp . rst) . bdy)
   #'(match exp (pat (match-xpnd rst . bdy)))))

(mac tbl-match (:keys)
  ((tbl ((:keys key ...) . bdy))
   #`(match tbl (($ <tbl>)
                 (w/keys (key ...) tbl
                         #,@#'bdy)))))

(mac w/keys
  (((key ...) tbl bdy)
   #`(w/ #,(splice
            (map (_fn (key)
                   #`(#,key (tbl '#,key)))
                 #'(key ...)))
       bdy)))

;;; TODO: only one body
(mac let-syn-vars
  ((vars pat body)
   #'(_let pat-ids (flatten (rec-filter sym? `(,(syn->dat #'pat))))
         (let-syn vars (map (_fn (id) (syn id #'pat)) pat-ids)
           body))))

;;; TODO: modularize
;;; TODO: add :as
(mac if-match (:or)
     ((exp ((pat :or val) bdy))
      (let-syn-vars pat-vars pat
                    #`(if exp (match exp (pat bdy))
                        (match val (pat bdy)))))
     ((exp (pat bdy))
      (let-syn-vars pat-vars pat
                    #`(if exp (match exp (pat bdy))
                          (_w/ #,(splice (zip #'pat-vars (map (const #f) #'pat-vars)))
                               bdy)))))

(mac op-match-xpnd
  ((() . bdy)
   #'(do . bdy))
  (((pat exp . rst) . bdy)
   #'(if-match exp (pat (op-match-xpnd rst . bdy)))))

;;; TODO: determine if these can be namespaced with base/def
(eval-when (expand load eval compile)
  (_def fn-match (exp)
    (syn-case exp ()
      (((pat ... . rst) b1 b2 ...)
       (let-syn pat:exps (parse-params #'(pat ...))
         #`((#,@(map cadr #'pat:exps) . rst)
            (match-xpnd #,(splice #'pat:exps) b1 b2 ...))))))
  
  ;; TODO: mac instead?
  (_def parse-params (params)
    (if (nil? params) params
        (syn-case params (:o :as)
          ((:o . rst)
           #`((#:o #:o) #,@(parse-params #'rst)))
          (((pat :as var :or val) . rst)
           #`(((pat :or val) var) #,@(parse-params #'rst)))
          (((pat :as var) . rst)
           #`((pat var) #,@(parse-params #'rst)))
          ((pat . rst) #`((pat #,(syn (gensym) #'pat))
                          #,@(parse-params #'rst))))))
  
  (_def parse-req-pat (pat)
    (syn-case pat ()
      ((pat #:as var) #'var)
      ((_) (syn (gensym) #'pat))))

  (_def flatten (lst)
    (append-map (_fn (elt)
                  (if (list? elt) (flatten elt)
                      `(,elt)))
                lst))

  (_def rec-filter (pred obj)
    (cond ((and (pair? obj) (nil? obj)) '(wha))
          ((pair? obj)
           (append (rec-filter pred (car obj))
                   (rec-filter pred (cdr obj))))
          ((pred obj) `(,obj))
          (else '())))
  
  (_def ids (pat)
    (flatten
     (loop lp ((for pat (in-list pat)))
       (cond ((identifier? pat) (list pat))
             ((list? pat) (ids pat)))
       pat)))
  (_def splice (lst)              ;TODO: use general ver.
    (reduce append '() (reverse lst))))
