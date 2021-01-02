(in-package "https://github.com/g000001/zrpaiprolog#internals")


#||
https://franz.com/support/documentation/current/doc/prolog.html
||#

(defun retract-same-arity-clause (clause)
  "Retract a clause from the data base"
  (let* ((head (clause-head clause))
         (pred (predicate head))
         (arity (1-  (length head))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete-if (lambda (x)
                       (let* ((h (clause-head x))
                              (p (predicate h))
                              (a (1- (length h))))
                         (and (eq p pred)
                              (= a arity))))
                     (get-clauses pred)))
    pred))


(defmacro <-- (&rest clause)
  "Retract a same arity clause from the data base,
and add a clause to the data base."
  (let ((clause (replace-?-vars clause)))
    `(progn
       (retract-same-arity-clause ',clause)
       (add-clause ',clause))))


(defun insert-deref (exp)
  (if (atom exp)
      (if (variable-p exp)
          `(deref-exp ,exp)
          exp)
      (cons (insert-deref (car exp))
            (insert-deref (cdr exp)))))


(defun prolog-translate-goals (goals)
  (mapcar (lambda (goal)
            (if (starts-with goal 'lisp)
                (let ((vars (variables-in (last goal))))
                  ``(,@',(butlast goal)
                         (apply ,(lambda (,@vars)
                                   ,@(insert-deref (last goal)))
                                (list ,@',vars))))
                `',goal))
          goals))


#+(or)
(let ((x 100) y)
  (prolog-compile 'TOP-LEVEL-QUERY
                  `(((TOP-LEVEL-QUERY)
                     (LISP ?A (APPLY ,(LAMBDA () X) (LIST)))
                     (= ?A ?B)
                     (LISP (APPLY ,(LAMBDA (?B) (SETF Y (+ (DEREF-EXP ?B) (DEREF-EXP ?B) X 1)))
                                  (LIST ?B)))))))
;;-> 
;;   (DEFUN TOP-LEVEL-QUERY/0 (CONT)
;;     (LET ((?B (?)) (?A (?)))
;;       (IF (UNIFY! ?A (APPLY #'(LAMBDA () (APPLY # (LIST))) 'NIL))
;;           (PROGN
;;            (APPLY #'(LAMBDA (?B) (APPLY # (LIST (DEREF-EXP ?B)))) (LIST ?A))
;;            (FUNCALL CONT))))) 
;;=> NIL


#+(or)
(defmacro prolog (&rest goals)
  (let ((goals (replace-?-vars goals)))
    `(block prolog
       (clear-predicate 'top-level-query)
       (add-clause (print `((top-level-query)
                       ,,@(prolog-translate-goals goals))))
       (run-prolog 'top-level-query/0 (constantly nil)))))

#|
(let ((x 100) y)
  (prolog (lisp ?a x)
          (= ?a ?b)
          (lisp (setf y (+ ?b ?b x 1))))
  y)
;;=> 301
|#
(defmacro prolog (&rest goals)
  "Run Prolog in the surrounding Lisp environment
which is accessed from lisp functor.
"
  (let ((goals (replace-?-vars goals))
        (*predicate* (gensym "anonymous-top-lavel-query")))
    `(block prolog
       (prolog-compile-symbols)
       (setf (fill-pointer *trail*) 0)
       (setf *var-counter* 0)
       (funcall
        (let ((*predicate* ',*predicate*)) ;***
          (compile
           nil
           `(lambda (cont)
              (block ,*predicate*
                .,(maybe-add-undo-bindings
                   (mapcar #'(lambda (clause)
                               (compile-clause () clause 'cont))
                           `((nil ,,@(prolog-translate-goals goals)))))))))
        (constantly nil)))))


;;; *EOF*
