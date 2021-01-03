(in-package "https://github.com/g000001/zrpaiprolog#internals")


#||
http://www.lispworks.com/documentation/lw71/KW-U/html/kwprolog-u-104.htm
||#

(defun make-return-exp (xpr)
  (typecase xpr
    (atom xpr)
    (cons `(list
            ,@(mapcar (lambda (x)
                        (if (variable-p x)
                            `(deref-exp ,x)
                            `(quote ,x)))
                      xpr)))))


;; (any '(?x is in (1 2 3)) '(member ?x (1 2 3)))
;; → (1 is in (1 2 3))
(defun any (pattern-to-instantiate goal-to-prove)
  (when (null pattern-to-instantiate)
    (error "must specify patterns."))
  ;; FIXME
  (eval
   `(prolog ,goal-to-prove
            (lisp (return-from prolog 
                    ,(make-return-exp pattern-to-instantiate))))))


;; (findall '(?x is in (1 2 3)) '(member ?x (1 2 3)))
;; → ((3 is in (1 2 3)) (2 is in (1 2 3)) (1 is in (1 2 3)))
(defun findall (pattern-to-instantiate goal-to-prove)
  (when (null pattern-to-instantiate)
    (error "must specify patterns."))
  ;; FIXME
  (eval
   (let ((result (gensym "result")))
     `(let (,result)
        (prolog
         ,goal-to-prove
         (lisp (push ,(make-return-exp pattern-to-instantiate)
                     ,result)))
        (nreverse ,result)))))


;; (findallset '?y '(or (= ?y 5) (= ?y 5)))
;; → (5)
(defun findallset (pattern-to-instantiate goal-to-prove)
  (when (null pattern-to-instantiate)
    (error "must specify patterns."))
  ;; FIXME
  (eval
   (let ((result (gensym "result"))
         (ans (gensym "ans")))
     `(let ((,result (make-hash-table :test #'equal)))
        (prolog
         ,goal-to-prove
         (lisp (setf (gethash ,(make-return-exp pattern-to-instantiate) ,result) 
                     T)))
        (let ((,ans (list)))
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (push k ,ans))
                   ,result)
          ,ans)))))


(defun logic (goal &key return-type all bag-exp)
  (ecase return-type
    ((:display nil)
     (let ((vars (variables-in goal)))
       (format *debug-io*
               "~{~{~S = ~S~}~%~}"
               (mapcan (lambda (v)
                         (mapcar (lambda (x)
                                   (list v x))
                                 (funcall (if all #'findall #'any) v goal)))
                       vars))))
    (:fill
     (if all
         (funcall (ecase all
                    (:values #'values-list)
                    (:list #'values))
                  (findall goal goal))
         (any goal goal)))
    (:bag 
     (if all
         (funcall (ecase all
                    (:values #'values-list)
                    (:list #'values))
                  (findall bag-exp goal))
         (any bag-exp goal)))
    (:alist
     (let ((vars (variables-in goal)))
       (if all
           (funcall (ecase all
                      (:values #'values-list)
                      (:list #'values))
                    (mapcan (lambda (v)
                              (mapcar (lambda (x)
                                        (cons v x))
                                      (findall v goal)))
                            vars))
           (mapcan (lambda (v)
                     (mapcar (lambda (x)
                               (cons v x))
                             (any (list v) goal)))
                   vars))))))


#||
(logic '(color ?x)
       :return-type :display
       :all :values)

(logic '(color ?x)
       :return-type :bag
       :bag-exp '(?x is a color)
       :all :list)

(logic '(color ?x)
       :return-type :alist
       :all nil)

((?x green)) 
 
(logic '(color ?x)
       :return-type :fill
       :all :values)
(findall '(color ?x) '(color ?x))

((color green) (color blue) (color red)) 

(defrel color
  ((color red))
  ((color blue))
  ((color green)))

||#

(defrel append
  ((append () ?x ?x))
  ((append (?x . ?xs) ?y (?x . ?zs))
   (append ?xs ?y ?zs)))


(defun make-return-exp* (xpr)
  (typecase xpr
    (atom xpr)
    (cons `(list
            ,@(mapcar (lambda (x)
                        (if (variable-p x)
                            `(deref-exp ',x)
                            x))
                      xpr)))))


(defmacro deflogfun (name (&rest args) sample-expr return-expr)
  `(defun ,name (,@args &key all)
     (logic (cons ',(car sample-expr)
                  ,(make-return-exp* (cdr sample-expr)))
            :return-type :bag
            :bag-exp ',return-expr
            :all all)))


#||
(deflogfun break-up (y) (append ?a ?b y) (?a ?b))

(break-up '(foo bar baz) :all :list)
→ ((nil (foo bar baz)) ((foo) (bar baz)) ((foo bar) (baz)) ((foo bar baz) nil))
||#

(defun ?.p (x)
  (typecase x
    (cl:symbol (eql 0 (search "?." (string x))))
    (T nil)))


(defun escape-?.var (exp)
  (cond ((null exp) nil)
        ((atom exp)
         (if (?.p exp)
             (intern (subseq (string exp) 2))
             `',exp))
        (T (list 'cons
                 (escape-?.var (car exp))
                 (escape-?.var (cdr exp))))))


(defmacro with-prolog (&body body)
  `(any T ,(escape-?.var `(and ,@body))))


#||
(defun palindromep (x)
  (with-prolog
   (append ?a (?b . ?c) ?.x) ; note "?.x" reference
    (or (reverse ?a ?c)
        (reverse ?a (?b . ?c)))))


(defrel reverse
  ((reverse () ()))
  ((reverse (?x . ?xs) ?ans)
   (reverse ?xs ?ans1)
   (append ?ans1 (?x) ?ans)))
||#

(defmacro defrel (name &body clauses)
  (destructuring-bind (first . rest)
                      clauses
    (let ((first (replace-?-vars first)))
      `(progn ',name
         (retract-same-arity-clause ',first)
         (add-clause ',first)
         ,@(mapcar (lambda (c)
                     `(add-clause ',(make-anonymous c)))
                   rest)))))


;;; *EOF*
