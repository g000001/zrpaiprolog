(cl:in-package asdf)


(defsystem zrpaiprolog
  :serial T
  :components ((:file "package")
               (:file "auxfns")
               (:file "patmatch")
               (:file "unify")
               (:file "prolog")
               (:file "prologc")
               (:file "prologcp")
               (:file "allegro-prolog-compat")
               (:file "common-prolog-compat")
               (:file "prolog-ext")))


;;; *EOF*
