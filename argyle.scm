(ns (argyle)
    :duplicates (last))

(use (argyle base)
     (argyle match)
     (argyle data)
     (argyle data tbl)
     (argyle data vec)
     (argyle data q)
     (argyle generic)
     (argyle loop)
     (argyle nested-loop)
     (argyle conc)
     (argyle reader)
     
     (ice-9 match)
     (ice-9 receive)
     (srfi srfi-45))

(re-export-ns
 (argyle base)
 (argyle match)
 (argyle data)
 (argyle data tbl)
 (argyle data vec)
 (argyle data q)
 (argyle generic)
 (argyle loop)
 (argyle nested-loop)
 (argyle conc)
 
 (ice-9 match)
 (ice-9 receive)
 (srfi srfi-45))
