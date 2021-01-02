(cl:in-package :cl)

(defpackage :unifgram
  (:use cl "https://github.com/g000001/zrpaiprolog"
   "https://github.com/g000001/zrpaiprolog#internals")
  (:shadowing-import-from :paiprolog.auxfns #:symbol #:debug)
  (:export #:rule
           #:-->
           #:--->
           #:==>
           #:add*))
