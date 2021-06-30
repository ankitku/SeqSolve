(in-package :z3)

;; Create a C array over elements of the given type and with the given length
(defmacro with-foreign-array ((array-var-name array-ty lisp-list ith-val &key (elt-var 'arg)) &body body)
  `(let ((array-len (length ,lisp-list)))
     (cffi:with-foreign-object
      (,array-var-name ',array-ty array-len)
      (loop for ,elt-var in ,lisp-list
            for i upto (1- array-len)
            do (setf (cffi:mem-aref ,array-var-name ',array-ty i) ,ith-val))
      ,@body)))

(defmacro with-foreign-arrays (array-specs &body body)
  (if array-specs
      `(with-foreign-array ,(car array-specs)
                           (with-foreign-arrays ,(cdr array-specs) ,@body))
    `(progn ,@body)))

(defun foreign-array-to-list (arr ty len)
  "Convert a foreign array of the given element type and length into a list."
  (loop for i below len
        collect (cffi:mem-aref arr ty i)))

#|
;; Write
(defmacro write-to-foreign-array (foreign-array element-type length lisp-list set-ith &key (elt-var 'elt) (idx-var 'i))
  `(loop for ,elt-var in ,lisp-list
         for ,idx-var below ,length
         do (setf (cffi:mem-aref ,foreign-array ,element-type ,idx-var)
                  ,set-ith)))
|#
