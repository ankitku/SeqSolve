(include-book "interface/itest-cgen")
(include-book "interface/itest-ithm")

:q

(load "~/quicklisp/setup.lisp")
(load "interface/acl2s-interface.lisp")
(load "interface/acl2s-highlevel-interface.lisp")

(pushnew (truename "../../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-lengths
  (:use :cl :z3 :acl2s :acl2s-interface))

(in-package :z3-lengths)
(import 'acl2s::(acl2s-compute))
(import 'acl2s-interface::(itest?-query))

;; When we ask ACL2s, we don't get any counterexamples...
(itest?-query 'acl2s::(implies (and (intp x) (intp y) (intp z) (intp a) (intp b)
                                    (< x 100)
                                    (< y 100)
                                    (< z 100)
                                    (< a 100)
                                    (< b 100))
                               (not (> (+ x y z a b) 490))))

;; What if we ask Z3?
(solver-init)
(z3-assert
 (x :int y :int z :int a :int b :int)
 (and (< x 100)
      (< y 100)
      (< z 100)
      (< a 100)
      (< b 100)
      (> (+ x y z a b) 490)))
(check-sat)

;; Just to double check, let's evaluate this in ACL2s
(acl2s-compute `(let ,(check-sat)
                  (and (< x 100)
                       (< y 100)
                       (< z 100)
                       (< a 100)
                       (< b 100)
                       (> (+ x y z a b) 490))))

