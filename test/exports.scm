;;; Every module under argyle/ loads, and every name it exports is bound.
;;; Catches things like exporting an imported name (which makes a fresh,
;;; unbound variable on Guile 3).  Run with: test/run exports

(use (argyle) (test check) (ice-9 ftw))

(def root (dirname (dirname (current-filename))))

;; Files that are included by other modules rather than being modules.
(def not-modules
  '("argyle/loop/loop.scm" "argyle/loop/nested-loop.scm"
    "argyle/loop/test-foof-loop.scm"))

(def scm-files (dir)
  (flatn (fn (e)
           (let p (str dir "/" e)
             (cond ((member e '("." "..")) '())
                   ((eq? 'directory (stat:type (stat (str root "/" p)))) (scm-files p))
                   ((string-suffix? ".scm" e) (list p))
                   (else '()))))
         (scandir (str root "/" dir))))

(def module-name (file)
  (map str->sym (string-split (string-drop-right file 4) #\/)))

(def unbound-exports (name)
  (let iface (resolve-interface name)
    (filter identity
            (module-map (fn (sym var) (and (~ (variable-bound? var)) sym)) iface))))

(for-each
 (fn (file)
   (unless (or (member file not-modules) (string-contains file "/private/")
               ;; needs guile-fibers, covered by fibers.scm
               (string=? file "argyle/fibers.scm"))
     (check (str file " exports are bound") '() (unbound-exports (module-name file)))))
 (sort (scm-files "argyle") string<?))

(done)
