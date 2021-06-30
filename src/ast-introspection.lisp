:q

(in-package "ACL2S")
(setq cc 0)

(defun new_xvar ()
  (setq cc (+ cc 1))
  (cons 'S (ACL2S::gen-sym-pkg (list 'X cc) "ACL2S")))

(defun new_yvar ()
  (setq cc (+ cc 1))
  (cons 'U (ACL2S::gen-sym-pkg (list 'X cc) "ACL2S")))

(new_yvar)

(defun mklvar (v)
  (if (tlp v)
      `(L . ,(second v))
    `(L . ,(cdr v))))

(defun mksvar (v)
  (if (tlp v)
      `(S . ,(second v))
    `(S . ,(cdr v))))

(defun mkuvar (v)
  (if (tlp v)
      `(U . ,(second v))
    `(U . ,(cdr v))))


(load "~/quicklisp/setup.lisp")
(pushnew (truename "../../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)
(in-package :z3)
(import acl2s::'(new_xvar new_yvar mklvar mksvar mkuvar cc))
(solver-init)
(defun convert-ast-to-list (ast ctx)
  (if (not (equal (z3-get-ast-kind ctx ast) :APP_AST))
      (ast-to-value ast ctx)
    (let* ((sort (z3-get-sort ctx ast))  
	   (sort-kind (z3-get-sort-kind ctx sort))
	   (decl (z3-get-app-decl ctx ast))
	   (decl-kind (z3-get-decl-kind ctx decl)))
      (match decl-kind
             (:OP_UNINTERPRETED
              (cond ((and (equal sort-kind :SEQ_SORT)
			  (zerop (z3-get-app-num-args ctx ast)))
		     (list :SVAR (intern (z3-get-symbol-string ctx (z3-get-decl-name ctx (z3-get-app-decl ctx ast))))))
		    ((and (equal sort-kind :INT_SORT)
			  (zerop (z3-get-app-num-args ctx ast)))
		     (list :IVAR (intern (z3-get-symbol-string ctx (z3-get-decl-name ctx (z3-get-app-decl ctx ast))))))
		    (t (error "unsupported uninterpreted function application"))))
	     
             (:OP_INTERNAL
              (ast-to-value ast ctx))
             (otherwise
              (cons (z3-get-decl-kind ctx (z3-get-app-decl ctx ast))
                    (loop for i below (z3-get-app-num-args ctx ast)
                          for arg = (z3-get-app-arg ctx ast i)
                          collect (convert-ast-to-list arg ctx))))))))
;; replace ... with whatever you want your package to be
;;(in-package ...)
;;(import 'z3::convert-ast-to-list)

;; some examples
(convert-ast-to-list (convert-to-ast '(= 7 27) nil nil *default-context*)
                     *default-context*)

(convert-ast-to-list
 (convert-to-ast '(= (+ x (* 3 5)) (+ 1 2 3))
                 (make-var-decls '(x :int) *default-context*)
                 nil *default-context*)
 *default-context*)

(convert-ast-to-list
 (convert-to-ast '(= (seq-concat "a" x "ba" x) (seq-concat y y "aba"))
                 (make-var-decls '(x :string y :string) *default-context*)
                 nil *default-context*)
 *default-context*)


(defun in (e xs)
  (cond ((endp xs) nil)
	((tree-equal e (car xs)) t)
	(t (in e (cdr xs)))))



(defun unique_cons (x ls)
  (cond ((in x ls) ls)
	(t (cons x ls))))

(unique_cons 3 '(1 2 3))
(unique_cons 4 '(1 2 3))

(defun unique_append (xs ys)
  (cond ((endp xs) ys)
	(t (unique_append (cdr xs) (unique_cons (car xs) ys)))))

(unique_append '(1 2 3) '(3 4 5))
(unique_append '(1 2 3) '(4 5))


(defun is_string_exp (asl)
  (cond ((stringp asl) t)
	((atom asl) nil)
	((integerp asl) nil)
	((member (car asl) '(:SVAR :OP_SEQ_CONCAT)) t)
        ((equal (car asl) :OP_EQ) (or (is_string_exp (second asl))
				      (is_string_exp (third asl))))
	(t nil)))

(is_string_exp '(:OP_EQ (:IVAR I0_2) (:OP_SUB 48 30)))
(is_string_exp '(:OP_EQ (:OP_SEQ_LENGTH (:SVAR PCTEMP_LHS_1)) 2))
(is_string_exp '(:OP_EQ (:SVAR T_5) (:OP_SEQ_CONCAT (:SVAR T_4) "UA-167675-3")))
(is_string_exp '(:OP_EQ (:SVAR |T_a|)
  (:OP_SEQ_CONCAT
   "&utmn=1130723541&utmcs=UTF-8&utmsr=1680x976&utmsc=24-bit&utmul=en-us&utmje=0&utmfl=-&utmdt=Ask%20A%20Word&utmhn=www.askaword.com&utmhid=1483079829&utmr=0&utmp="
   (:SVAR T_9))))
			


(defun zip_append (ls1 ls2)
  (mapcar (lambda (x) (append (first x) (second x))) (mapcar #'list ls1 ls2)))

(zip_append '((1 2 3) (4 5 6) (7 8 9)) '((4 5 6) (7 8 9) (1 2 3)))



;; create new result type
;; '(seq qs lia-eqs cs xs ys is)
(defun flatten_seq (l)
  (cond ((stringp l) (let* ((constlist (coerce l 'list))
			    (unique_cl (remove-duplicates constlist)))
		       (list constlist nil nil unique_cl nil nil nil)))
	(t (let ((lcl (list (car l))))
	     (trivia:match lcl
			   ('(:SVAR) (let* ((v (mksvar l)))
				       (list `(,v) nil nil nil `(,v) nil nil)))
			   ('(:OP_SEQ_UNIT) (let* ((e (mkuvar (second l))))
					      (list `(,e) nil nil nil nil `(,e) nil)))
			   ('(:OP_SEQ_CONCAT) (let* ((res1 (flatten_seq (second l)))
						     (res2 (flatten_seq (third l))))
						(append (list (append (car res1) (car res2)))
							(zip_append (cdr res1) (cdr res2)))))
			   ('(:OP_SEQ_AT) (let* ((x (new_xvar))
						 (e (new_yvar))
						 (y (new_xvar)))
					    (trivia:match (flatten_seq (second l))
							  ((list seq qs lia-eqs cs xs ys is)
							   (list `(,e)
								 (cons (cons seq `(,x ,e ,y))
								       qs)
								 (cons `(= ,(mklvar x) ,(third l))
								       lia-eqs)
								 cs
								 (append `(,x ,y) xs)
								 (cons e ys)
								 is)))))
			   
			   (t `(,l nil nil nil nil nil nil)))))))

	
(flatten_seq "ankit")
(flatten_seq '(:OP_SEQ_CONCAT (:OP_SEQ_UNIT (:IVAR |b|)) "ankit"))
(flatten_seq '(:OP_SEQ_AT "ankit" 2))



(defun to_len (x)
  (cond ((acl2s::vp x) (mklvar x))
	(t 1)))

;; create new result type
;; '(iexp qs lia-eqs cs xs ys is)
(defun flatten_len (l)
  (cond ((numberp l) (list l nil nil nil nil nil nil))
	((member (car l) '(:OP_ADD :OP_SUB))
	 (let ((m (list (flatten_len (second l))
			(flatten_len (third l)))))
	   (trivia:match m
			 ((list (list iexp1 qs1 lia-eqs1 cs1 xs1 ys1 is1)
				(list iexp2 qs2 lia-eqs2 cs2 xs2 ys2 is2))
			  (list (list (car l) iexp1 iexp2)
			        (unique_append qs1 qs2)
				(unique_append lia-eqs1 lia-eqs2)
				(unique_append cs1 cs2)
				(unique_append xs1 xs2)
				(unique_append ys1 ys2)
				(unique_append is1 is2))))))
	(t (let ((lcl (list (car l))))
	     (trivia:match lcl
			   ('(:OP_SEQ_LENGTH) (trivia:match (flatten_seq (second l))
							    ((list seq1 qs1 lia-eqs1 cs1 xs1 ys1 is1)
							     (list (cons '+ (mapcar 'to_len seq1))
								   qs1 lia-eqs1 cs1 xs1 ys1 is1))))
			   ('(:IVAR)  (let ((ix (mklvar l)))
					(list ix nil nil nil nil nil (list ix))))
			   ('(:OP_SEQ_INDEX)
			    (let* ((x (new_xvar))
				   (y (new_xvar)))
			      (trivia:match (flatten_seq (second l))
					    ((list seq qs lia-eqs cs xs ys)
					     (list (mklvar x)
						   (cons (cons seq (list x (third l) y)) qs)
						   lia-eqs
						   cs
						   (append `(,x ,y) xs)
						   ys
						   nil)))))
			   (_ (let ((res (list (flatten_len (second l))
					       (flatten_len (third l)))))
				(trivia:match res
					      ((list res1 res2))
					      (cons (list (car l) (car res1) (car res2))
						    (zip_append (cdr res1)
								(cdr res2)))))))))))	   


(defun trx (l)
  (reduce (lambda (l1 r) (subst (cdr r) (car r) l1)) '((:OP_EQ . =)
						  (:OP_LT . <)
						  (:OP_GT . >)
						  (:OP_LE . <=)
						  (:OP_GE . >=)
						  (:OP_ADD . +)
						  (:OP_SUB . -))
	  :initial-value l))



					    
;;(qs lia-eqs cs xs ys is)
(defun parse_one (l)
  (cond ((and (is_string_exp l)
	      (equal (car l) :OP_EQ))
	 (let* ((res1 (flatten_seq (second l)))
		(res2 (flatten_seq (third l))))
	   (cons (cons (cons (car res1) (car res2))
		       (append (second res1) (second res2)))
		 (zip_append (cddr res1) (cddr res2)))))
	((and (is_string_exp l)
	      (equal (car l) :OP_SEQ_CONTAINS))
	 (let* ((res1 (flatten_seq (second l)))
		(res2 (flatten_seq (third l)))
		(x (new_xvar))
		(y (new_xvar))
		(eq1 (cons (car res1)
			   (append `(,x) (car res2) `(,y))))
		(qs1 (cons eq1 (append (second res1)
				       (second res2)))))
	   (cons qs1 (zip_append (cddr res1) (cddr res2)))))
	((member (car l) '(:OP_EQ :OP_GE :OP_GT :OP_LE :OP_LT :OP_ADD :OP_SUB))
	 (let ((m (list (flatten_len (second l)) (flatten_len (third l)))))
	   (trivia:match m
			 ((list (list iexp1 qs1 lia-eqs1 cs1 xs1 ys1 is1)
				(list iexp2 qs2 lia-eqs2 cs2 xs2 ys2 is2))
			  (list (append qs1 qs2)
				(trx (cons (list (car l) iexp1 iexp2)
					   (unique_append lia-eqs1 lia-eqs2)))
				(unique_append cs1 cs2)
				(unique_append xs1 xs2)
				(unique_append ys1 ys2)
				(unique_append is1 is2))))))))

(solver-init)
;;(qs lia-eqs cs xs ys is)
(defun parse (f j)
  (let* ((ast-vec (parse-smt2-file f))
	 (ast-list  (ast-vector-to-list ast-vec))
	 (llist (mapcar (lambda (ast) (convert-ast-to-list ast *default-context*))
			ast-list))
	 (x (setq cc j))
	 (res (reduce (lambda (l1 l2) (zip_append l1 (parse_one l2))) llist
		      :initial-value '(nil nil nil nil nil nil))))
    (cons acl2s::cc
	  res)))


(parse_one '(:OP_EQ 1 (:OP_SEQ_LENGTH (:SVAR |e|))))

(parse_one '(:OP_EQ (:OP_SEQ_CONCAT (:SVAR |e|)) (:OP_SEQ_CONCAT (:SVAR |e|))))

;(mapcar (lambda (ast) (convert-ast-to-list ast *default-context*))
;	(ast-vector-to-list
;	 (parse-smt2-file "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/lengths-concats/lengths-concats-00001-0.smt25")))


;(setq cc 0)

;(parse "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/lengths-concats/lengths-concats-00001-0.smt25" 0)

;(parse "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/concats-extracts-big/concats-extracts-big-00081-0.smt25" 0)

;(parse "/Users/ankitku/dev/monoid-solver/evaluation/filtered/sat/withlen/3464.corecstrs.readable.smt2" 0)
 
;(parse "/Users/ankitku/dev/monoid-solver/evaluation/filtered/sat/withlen/3464.corecstrs.readable.smt2" 0)

;(parse "/Users/ankitku/dev/monoid-solver/evaluation/filtered/unsat/small/22637.corecstrs.readable.smt2" 0)

;(parse "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/lengths-concats/lengths-concats-00050-1.smt25" 3)

;(parse_one '(:OP_EQ (:SVAR T_9) (:OP_SEQ_CONCAT (:SVAR T1_20) (:SVAR T2_20))))

;(parse "/Users/ankitku/dev/monoid-solver/evaluation/kaluza/unsat/small/22625.corecstrs.readable.smt2")
;(parse "/Users/ankitku/dev/monoid-solver/evaluation/remora/rem1.smt2" 0)
