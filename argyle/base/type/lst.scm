(ns (argyle base type lst)
    :use-module (srfi srfi-1)
    :re-export-and-replace (filter reduce list-index))
(use (argyle base fn))

(defp empty? null?)
(defp unique delete-duplicates)
(defp unique! delete-duplicates!)
(defp range iota)
