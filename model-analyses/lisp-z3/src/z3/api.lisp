(in-package :z3)

;; TODO: integrate defdata's types where possible
;; e.g. automatically convert defdatas into "equivalent" Z3 sorts
;; but a fair amount of work is needed here to convert back and forth between z3 values and defdata values.

(defun make-var-decls (decls context)
    "Translate a user-provided list of variable and function
declarations into a variable sort alist for internal use."
  (loop for (var ty) on decls by #'cddr
        unless (and (consp ty) (equal (car ty) :fn))
        collect (cons var (make-instance 'sort
                                         :handle (get-sort ty context)
                                         :context context))))

;;(make-var-decls '(x :int y :bool) *default-context*)

(defun make-fn-decls (decls context)
  "Translate a user-provided list of variable and function
declarations into a function declaration alist for internal use."
  (loop for (var ty) on decls by #'cddr
        when (and (consp ty) (equal (car ty) :fn))
        collect (list var (make-fn-decl var (second ty) (third ty) context) ty)))

;; TODO: we may want to use z3-mk-fresh-func-decl to avoid name
;; clashes with builtin functions. This would also require changes to
;; the model translation code.
;; TODO: weird SBCL note when compiled:
;; note: Type assertion too complex to check:
;; (VALUES Z3::FUNC-DECL &REST T).
;; It allows an unknown number of values, consider using
;; (VALUES Z3::FUNC-DECL &OPTIONAL).
(declaim (ftype (function (symbol * * context) func-decl) make-fn-decl))
(defun make-fn-decl (name domain range context)
  "Given a name, a list of domain sort specifiers, and a range sort specifier, create an uninterpreted func-decl with that name and signature."
  (with-foreign-array (domain-sorts-array z3-c-types::Z3_sort domain (get-sort arg context))
                      (make-instance 'func-decl
                                     :handle (z3-mk-func-decl context
                                                              (z3-mk-string-symbol context (symbol-name name))
                                                              (length domain)
                                                              domain-sorts-array
                                                              (get-sort range context))
                                     :context context)))

(cffi:defcallback error-handler :void ((ctx z3-c-types:context) (error-code z3-c-types:error_code))
                  (restart-case
                   (match error-code
                          (:OK (error "Z3: error handler called with error code OK - should not occur."))
                          (:SORT_ERROR (error "Z3: tried to build an AST that is not well-sorted"))
                          (:IOB (error "Z3: index out of bounds"))
                          (:INVALID_ARG (error "Z3: Invalid argument was provided"))
                          (:NO_PARSER (error "Z3: parser output is not available"))
                          (:INVALID_PATTERN (error "Z3: invalid pattern used to build a quantifier"))
                          (:MEMOUT_FAIL (error "Z3: unable to allocate memory"))
                          (:FILE_ACCESS_ERROR (error "Z3: unable to access file"))
                          (:INTERNAL_FATAL (error "Z3: internal error occurred"))
                          (:DEC_REF_ERROR (error "Z3: Tried to decrement the reference counter of an AST that was deleted or the reference counter was not initialized with Z3_inc_ref."))
                          (:INVALID_USAGE (error "Z3: API call is invalid in the current state: ~a" (z3-get-error-msg ctx error-code)))
                          (:PARSER_ERROR (error "Z3: An error occurred when parsing a string or file: ~a" (z3-get-error-msg ctx error-code)))
                          (:EXCEPTION (error "Z3: An exception occurred: ~a" (z3-get-error-msg ctx error-code)))
                          #| (let ((error-msg (z3-get-error-msg ctx error-code)))
                             (format t "Z3 exception ~S" error-msg)))|#
                          (otherwise (error "Z3: an unknown error occurred with code ~S" error-code)))
                   (ignore-and-continue () :report "Ignore the error and return control to Z3." nil)))

(defun solver-init ()
  "Initialize the Z3 interface with a context and a solver."
  (setf *default-context* (make-instance 'context))
  (z3-set-error-handler *default-context* (cffi:callback error-handler))
  (setf *default-solver* (make-simple-solver *default-context*)))

(defun solver-push (&optional solver)
  "Create a new scope. This is useful for incremental solving."
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (z3-solver-push ctx slv)))

(defun solver-pop (&key (solver nil) (n 1))
  "Pop one or more scopes off the Z3 stack. This essentially undoes
any Z3 declarations or assertions that occurred between the relevant
'push' operation and this 'pop' operation."
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (unless (<= n (z3-solver-get-num-scopes ctx slv))
      (error "You can't pop ~S level(s) - the solver is currently at level ~S" n (z3-solver-get-num-scopes ctx slv)))
    (z3-solver-pop ctx slv n)))

(defun solver-reset (&optional solver)
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (z3-solver-reset ctx slv)))

(defun z3-assert-fn (var-decls stmt &optional solver)
  ;; TODO we do nicer error handling here
  (when (oddp (length var-decls)) (error "Each declared variable must have a type."))
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (solver-assert
     slv
     (convert-to-ast stmt (make-var-decls var-decls ctx) (make-fn-decls var-decls ctx) ctx))))

(defmacro z3-assert (var-decls stmt &optional solver)
  `(z3-assert-fn ',var-decls ',stmt ,solver))

(defun get-model (&optional solver)
  "Get the model object for the last solver-check[-assumptions] call.
   Will invoke the error handler if no model is available."
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (make-instance 'model
                   :handle (z3-solver-get-model ctx slv)
                   :context ctx)))

(defun check-sat (&optional solver)
  "Ask Z3 to check satisfiability of the global assertion stack.
Returns either :UNSAT, :UNKNOWN, or a (possibly empty) list of
bindings corresponding to the model that Z3 generated."
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (match (z3-solver-check ctx slv)
           (:L_TRUE ;; assertions are satisfiable (a model may be generated)
            (append (model-constants-to-assignment (get-model solver) ctx)
                    (model-funcs (get-model solver) ctx)))
           (:L_FALSE :UNSAT) ;; assertions are not satisfiable (a proof may be generated)
           ;; TODO: in the unknown case we may want to get the model and see if the assignment satisfies the assertions
           ;; if so we can return it.
           (:L_UNDEF :UNKNOWN)))) ;; get_model may succeed but the model may not satisfy the assertions
