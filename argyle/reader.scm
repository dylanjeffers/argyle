(ns (argyle reader))
(use ((argyle base) :hide (str))
     (argyle loop)
     (argyle generic base)
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
    `(,(list-ref ctors (index-of strts strt string=?))))        ; zero for now
  (def parse (obj prt #:o (buff '()))
    (if (nil? buff)
        (let nxt (get-datum prt)
          (cond ((eof-object? nxt) obj)
                ((symbol? nxt) (parse obj prt (sym-buff nxt)))
                ;; 'x] reads as (quote x]): split the symbol and keep
                ;; only its first piece quoted.
                ((quoted-sym? nxt)
                 (let pieces (sym-buff (cadr nxt))
                   (parse (cons `(,(car nxt) ,(->dat (car pieces))) obj)
                          prt (cdr pieces))))
                (else (parse (cons nxt obj) prt))))
        (let (nxt rst) (snoc buff) 
          (cond ((end? ends nxt) (values obj rst))
                ((strt? strts nxt)
                 (let (obj* buff) (parse (mke-struct nxt) prt rst)
                   (parse (cons (rev obj*) obj) prt buff)))
                (else (parse (cons (->dat nxt) obj) prt rst))))))
  (read-hash-extend chr
    (fn (chr prt)
      (let (obj buff) (parse (mke-struct (str chr)) prt)
        ;; A nested literal can end mid-token, e.g. the inner #[ in
        ;; #[1 #[2 3]] reads "3]]"; hand the rest back to the outer reader.
        (unless (nil? buff)
          (unread-string (apply string-append buff) prt))
        (rev obj)))))

(def quoted-sym? (obj)
  (and (pair? obj) (memq (car obj) '(quote quasiquote unquote))
       (pair? (cdr obj)) (symbol? (cadr obj))))

(def strt? (strts obj) (and (string? obj) (or-map (fn (strt) (string=? obj strt)) strts)))
(def end? (ends obj) (and (string? obj) (or-map (fn (end) (string=? obj end)) ends)))

;;; TODO: generalize aned add to lst.scm
(def index-of (lst obj eq?)
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
  (aif (string->number str) it (string->symbol str)))

;;; backwards cons :)
(def snoc (lst)
  (if (nil? lst) (values '() '()) 
      (values (car lst) (cdr lst))))

(xtnd-readr #\[ '(vec tbl) '("[" "{") '("]" "}"))
(xtnd-readr #\{ '(vec tbl) '("[" "{") '("]" "}"))

;;; TODO: move to conc.scm?
(read-hash-extend #\@
  (fn (chr prt)
    `(@ ,(get-datum prt))))

(read-hash-extend #\~
  (fn (chr prt)
    `(~ ,(get-datum prt))))
