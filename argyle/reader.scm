(ns (argyle reader))
(use ((argyle base) :hide (str))
     (argyle loop)
     (argyle generic)
     (argyle guile)                    ; tmp
     (argyle data tbl)
     (argyle data vec)
     (argyle conc)
     (ice-9 regex)
     (ice-9 match)                      ; tmp
     (rnrs io ports)
     ((srfi srfi-1) :select (take reduce zip delete-duplicates drop)))

(read-disable 'square-brackets)

(def xtnd-readr (chr ctors #:o strts ends)
  (def regx (apply string-append `("[" ,@ends ,@strts "]")))
  (def sym-buff (sym-exp) (str-split regx (str sym-exp)))
  (def mke-struct (#:o (strt "["))
    `(,(list-ref ctors (lst-idx strts strt string=?))))        ; zero for now
  (def parse (obj prt #:o (buff '()))
    (if (nil? buff)
        (let nxt (get-datum prt)
          (cond ((eof-object? nxt) obj)
                ((sym? nxt) (parse obj prt (sym-buff nxt)))
                (else (parse (cons nxt obj) prt))))
        (let (nxt rst) (snoc buff) 
          (cond ((end? ends nxt) (vals obj rst))
                ((strt? strts nxt)
                 (let (obj* buff) (parse (mke-struct nxt) prt rst)
                   (parse (cons (rev obj*) obj) prt buff)))
                (else (parse (cons (->dat nxt) obj) prt rst))))))
  (read-hash-extend chr
    (fn (chr prt)
      (let (obj buff) (parse (mke-struct (str chr)) prt)
        (if (nil? buff) (rev obj)
            (error "reader broke, alpha software :("))))))

(def strt? (strts obj) (and (str? obj) (or-map (fn (strt) (string=? obj strt)) strts)))
(def end? (ends obj) (and (str? obj) (or-map (fn (end) (string=? obj end)) ends)))

;;; TODO: generalize aned add to lst.scm
(def lst-idx (lst obj eq?)
  (loop ((for obj* rst (in-list lst))
         (where idx 0 (1+ idx))
         (until (eq? obj* obj)))
      => (if (nil? rst) #f idx)))

(def str-split (regx str)
  (let matchs (list-matches regx str)
    (w/ (strts (map match:start matchs)
         ends (map match:end matchs)
         idxs `(0 ,@(splice (zip strts ends)) ,(len str)))
      ;; TODO: use until
      (loop lp ((idxs (delete-duplicates idxs)))
        (if (< (_length idxs) 2) '()
            `(,(substring str (car idxs) (cadr idxs))
              ,@(lp (cdr idxs))))))))

(def splice (lst)
  (reduce join '() (rev lst)))

(def ->dat (str)
  (aif (str->num str) it (sym str)))

;;; backwards cons :)
(def snoc (lst)
  (if (nil? lst) (vals '() '()) 
      (vals (car lst) (cdr lst))))

(xtnd-readr #\[ '(vec tbl) '("[" "{") '("]" "}"))
(xtnd-readr #\{ '(vec tbl) '("[" "{") '("]" "}"))

;;; TODO: move to conc.scm?
(read-hash-extend #\@
  (fn (chr prt)
    `(@ ,(get-datum prt))))

(read-hash-extend #\~
  (fn (chr prt)
    `(~ ,(get-datum prt))))
