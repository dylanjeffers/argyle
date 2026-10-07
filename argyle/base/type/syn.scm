(ns (argyle base type syn)
    :export (syn? syn->dat dat->syn))
(use (argyle base fn)
     (argyle guile)
     ((system syntax) :select (syntax?)))

;;; Guile 3 syntax objects are their own type, no longer tagged vectors.
(def syn? syntax?)

(def syn->dat syntax->datum)
(def dat->syn datum->syntax)
