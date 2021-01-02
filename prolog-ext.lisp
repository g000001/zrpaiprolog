(in-package "https://github.com/g000001/zrpaiprolog#internals")


(defmacro prolog-collect ((&rest vars) &body body)
  "collect all bindings of vars"
  (when (null vars)
    (error "must specify vars."))
  (let ((result (gensym "result")))
    `(let (,result)
       (prolog
        ,@body
        ,@(when vars
            `((lisp (push ,(if (length=1 vars)
                               (car vars)
                               `(list ,@vars))
                          ,result)))))
       ,result)))


(defmacro prolog-first ((&rest vars) &body body)
  "return first bindding of vars"
  (when (null vars)
    (error "must specify vars."))
  `(prolog
     ,@body
     (lisp (return-from prolog ,(if (length=1 vars)
                                    (car vars)
                                    `(values ,@vars))))))


(defun add-object-clause (name obj &key asserta)
  (let ((pred name))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (if asserta
	      (nconc (list (list (list name obj))) (get-clauses pred))
	      (nconc (get-clauses pred) (list (list (list name obj))))))
    pred))


(defun reset-object-clauses (name)
  (setq *db-predicates* (remove name *db-predicates*))
  (setf (get name 'clauses) '()))


;;; *EOF*
