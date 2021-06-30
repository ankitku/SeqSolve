(include-book "seqsolve")
(defttag :z3)
(acl2::subsume-ttags-since-defttag)
(acl2::include-raw "z3_raw_code.lsp" :host-readtable t)
(acl2::include-raw "ast-introspection.lisp" :host-readtable t)
(acl2::include-raw "seq_raw_code.lsp" :host-readtable t)

:q

(defun build-seqsolve ()
(save-exec "seqsolve_exec" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(acl2s::main_cl (cdr sb-ext:*posix-argv*))' --disable-debugger"))

(defun build-eval-seqsolve ()
(save-exec "eval_seqsolve_servm_exec" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(acl2s::eval_dir (cdr sb-ext:*posix-argv*))' --disable-debugger"))

(build-seqsolve)
;(build-eval-seqsolve)


