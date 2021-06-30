(in-package :z3)

(defun model-constants-to-assignment (model ctx)
  "Retrieve constant interpretations from the given model."
  ;; This is a two step process:
  ;; 1. Get constant declarations (note that these are function declarations with 0 parameters) from the model
  ;; 2. Get the interpretations of each of the constant declarations from the model and convert into Lisp forms.
  (let ((const-decls
         (loop for i below (z3-model-get-num-consts ctx model)
               collect (z3-model-get-const-decl ctx model i))))
    (loop for decl in const-decls
          for name = (z3-get-symbol-string ctx (z3-get-decl-name ctx decl))
          for value = (z3-model-get-const-interp ctx model decl)
          ;; TODO: will this catch a null pointer? Need a test case...
          when (not (equal value 0)) ;; may return null if the model doesn't assign an interpretation for the func-decl.
          ;; TODO: what should we do if model doesn't assign an interpretation? Currently we silently skip.
          collect (list (intern name) (ast-to-value value ctx)))))

(defun convert-func-entry (entry ctx)
  "Convert a function interpretation entry into a cons pair."
  (z3-func-entry-inc-ref ctx entry)
  (let ((args
         (loop for i below (z3-func-entry-get-num-args ctx entry)
               collect (ast-to-value (z3-func-entry-get-arg ctx entry i) ctx)))
        (val (ast-to-value (z3-func-entry-get-value ctx entry) ctx)))
    (z3-func-entry-dec-ref ctx entry)
    (cons args val)))

(defun func-interp-to-alist (interp ctx)
  "Translate a function intepretation into an alist."
  (let ((num-entries (z3-func-interp-get-num-entries ctx interp)))
    (cons (cons :default (ast-to-value (z3-func-interp-get-else ctx interp) ctx))
          (loop for i below num-entries
                for entry = (z3-func-interp-get-entry ctx interp i)
                collect (convert-func-entry entry ctx)))))

(defun model-funcs (model ctx)
  "Retrieve function interpretations from the given model."
  ;; Basically there are three steps:
  ;; 1. Get the function declarations from the model
  ;; 2. Use the function declarations to get the functions interpretations from the model
  ;; 3. Convert the function interpretations into Lisp forms (we translate into alists)
  (let* ((func-decls
          (loop for i below (z3-model-get-num-funcs ctx model)
                collect (z3-model-get-func-decl ctx model i))))
    (loop for decl in func-decls
          for interp = (z3-model-get-func-interp ctx model decl)
          for name = (z3-get-symbol-string ctx (z3-get-decl-name ctx decl))
          collect (list (intern name)
                        (list 'quote (list :fn
                                           ;; TODO: it might be better to return the user-provided sort specifiers for this function. But this will work for now.
                                           (list (func-decl-domain decl ctx) (func-decl-range decl ctx))
                                           (func-interp-to-alist interp ctx)))))))
