(ns (argyle base type strm))
(use (argyle base fn)
     (argyle base mac)
     (argyle lib streams))
(export scons sfn slet strm-match strm-of)

(defp strm? stream?)
(defp strm->lst stream->list)
(defp scar stream-car)
(defp scdr stream-cdr)
(defp strm-join stream-concat)
(defp strm-const stream-constant)
(defp strm-drop stream-drop)
(defp strm-fltr stream-filter)
(defp strms-fld stream-fold)
(defp strm-each stream-for-each)
(defp strm-frm stream-from)
(defp itr stream-iterate)
(defp strm-len stream-length)
(defp strm-map stream-map)
(defp snil stream-null)
(defp snil? stream-null?)
(defp strm-pair stream-pair?)
(defp strm-range stream-range)
(defp strm-ref stream-ref)
(defp strm-rev stream-reverse)
(defp strm-scan stream-scan)
(defp strm-take stream-take)
(defp strm-unfld stream-unfold)
(defp strm-zip stream-zip)

(mac scons
  ((obj strm) #'(stream-cons obj strm)))

(mac sfn
  ((args e1 e2 ...) #'(stream-lambda args e1 e2 ...)))

(mac slet
  ((tag ((name val) ...) e1 e2 ...)
   #'(stream-let tag ((name val) ...) e1 e2 ...)))

(mac strm-match
  ((strm-exp (pat . exp) ...)
   #'(stream-match strm-exp (pat . exp) ...)))

(mac strm-of
  ((exp rst ...) #'(stream-of exp rst ...)))
