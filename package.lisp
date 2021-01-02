(cl:in-package cl-user)


(defpackage "https://github.com/g000001/zrpaiprolog"
  (:nicknames zrlog zrpaiprolog)
  (:use)
  (:shadowing-import-from cl
   and >= integer number write real atom = or if catch > < read close throw char-code)
  (:export #:?-
           #:<-
           #:<--
           #:?
           #:fail
           #:true
           #:call
           #:!
           #:and
           #:or
           #:if
           #:catch
           #:throw
           #:=
           #:unify
           #:unify-with-occurs-check
           #:\\=
           #:var
           #:atom
           #:integer
           #:real
           #:atomic
           #:compound
           #:nonvar
           #:number
           #:==
           #:\\==
           #:@<
           #:@=<
           #:@>2
           #:@>=
           #:functor
           #:arg
           #:copy-term
           #:is
           #:|=:=|
           #:=\\=
           #:<
           #:=<
           #:>
           #:>=
           #:clause
           #:current-predicate
           #:asserta
           #:assertz
           #:retract
           #:abolish
           #:findall
           #:bagof
           #:current-input
           #:current-output
           #:set-input
           #:set-output
           #:close
           #:flush-output
           #:stream-property
           #:at-end-of-stream
           #:set-stream-position
           #:get-char
           #:put-char
           #:nl
           #:get-code
           #:put-code
           #:read
           #:write
           #:fail-if
           #:once
           #:repeat
           #:atom-length
           #:atom-concat
           #:sub-atom
           #:atom-chars
           #:atom-codes
           #:atom-characters
           #:string-atom
           #:string-list
           #:char-code
           #:number-chars
           #:number-codes
           #:lisp
           #:prolog
           #:prolog-collect
           #:prolog-first
           #:add-object-clause
           #:reset-object-clauses))


(defpackage "https://github.com/g000001/zrpaiprolog#internals"
  (:use cl "https://github.com/g000001/zrpaiprolog")
  (:shadow #:symbol #:debug)
  (:export #:once-only
           #:side-effect-free?
           #:funcall-if
           #:read-time-case
           #:rest2
           #:find-anywhere
           #:starts-with
           #:find-all-if
           #:find-all
           #:partition-if
           #:maybe-add
           #:seq-ref
           #:maybe-set-fill-pointer
           #:symbol
           #:new-symbol
           #:last1
           #:mappend
           #:mklist
           #:flatten
           #:random-elt
           #:member-equal
           #:compose
           #:dbg
           #:debug
           #:undebug
           #:dbg-indent
           #:fail
           #:no-bindings
           #:pat-match
           #:match-variable
           #:make-binding
           #:binding-var
           #:binding-val
           #:get-binding
           #:lookup
           #:extend-bindings
           #:variable-p
           #:defun-memo
           #:memo
           #:memoize
           #:clear-memoize
           #:delay
           #:force
           #:defresource
           #:with-resource
           #:queue-contents
           #:make-queue
           #:enqueue
           #:dequeue
           #:front
           #:empty-queue-p
           #:queue-nconc
           #:sort*
           #:reuse-cons
           #:length=1
           #:rest3
           #:unique-find-if-anywhere
           #:find-if-anywhere
           #:define-enumerated-type
           #:not-null
           #:first-or-nil
           #:first-or-self))


;;; *EOF*
