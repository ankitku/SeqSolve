:q
(load "~/quicklisp/setup.lisp")
(pushnew (truename "../../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)
(ql:quickload :log4cl)
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)
(ql:quickload :jsown)
(ql:quickload :trivia)
(ql:quickload :inferior-shell)

(in-package "ACL2S")

(import z3::'(solver-init solver-push solver-pop check-sat :unsat parse))
(import acl2s::'(blockp svarp uvarp vp ap eqp qsp sub2 sub2-all
                        hons-get hons-acons len eqelim seqlen
                        countc lvar nvar evar lvarp fvarp begin_varp
                        end_varp compress compress-set
                        trim rem-dups* new-len-var new-string-var
                        set-empty-vs eq-len sum-nvars v-sumconsts eq-consts
                        eq-flips declxp
                        +int-vars-decl+ +seq-len-vars-pos+ nvar-decl
                        +seq-countc-vars-pos+ +bool-vars-decl+
                        +len-vars-pos+ v-in init-len-v init-consts-v))

(shadowing-import trivia::'(guard))

(defvar *len_constraints* t)
(defvar *const_constraints* t)
(defvar *flip_constraints* t)

;;-----------------------------------------------------------
;; Solver -> Z3 functions
;;-----------------------------------------------------------  

(defun solver->z3 (c)
  (intern (format nil "~a-~a" (car c) (cdr c))))

(defun solver->nz3 (c)
  (intern (format nil "~a-~a-~a"
                  (first c)
                  (second c)
                  (third c))))

(solver->z3 '(L . X))
(solver->z3 '(L . X_1))

(defun solver->fz3 (c)
  (intern (format nil "~a-~a-~a-~a"
                  (first c)
                  (second c)
                  (third c)
                  (fourth c))))

(defun solver->z3exp (e)
  (cond ((atom e) e)
        ((lvarp e) (solver->z3 e))
        ((evarp e) (solver->z3 e))
        ((or (nvarp e)
             (begin_varp e)
             (end_varp e)) (solver->nz3 e))
        ((fvarp e) (solver->fz3 e))
        (t (cons (solver->z3exp (car e))
                 (solver->z3exp (cdr e))))))

(seqlen '((S . X) #\1 #\2 #\3 (#\5 . (+ (L . X) 3))))

(solver->z3exp '(L . X_1))
(solver->z3exp '(L . X))


(solver->z3exp (seqlen '((S . X) #\1 #\2 #\3 (#\5 . (+ (L . X) #\3)))))
(solver->z3exp (seqlen '((S . X_1) #\1 #\2 #\3 (#\5 . (+ (L . X_1) #\3)))))


(defun z3->solver (v) 
  (let ((s (cl-ppcre::split "-" (symbol-name v)))) 
    (cons (read-from-string (format nil "~a" (car s)))
          (read-from-string (format nil "~a" (second s))))))

(z3->solver '|L-var639|)



;;-----------------------------------------------------------
;; Z3 interaction functions
;;-----------------------------------------------------------

(defun add-assertion (stmt)
  (let* ((lses (collect-vs stmt))
         (ls (first lses))
         (es (second lses)))
    ; (print (cons (solver->z3exp
    ;               (append (+int-vars-decl+ ls)
    ;                       (+bool-vars-decl+ es)))
    ;              (solver->z3exp stmt)))
    (z3-assert-fn (solver->z3exp
                   (append (+int-vars-decl+ ls)
                           (+bool-vars-decl+ es)))
                  (solver->z3exp stmt))))


(z3::solver-init)
(add-assertion t)
(check-sat)

(add-assertion '(if t t t))

(defun init-len (vs)
  (mapcar (lambda (v) (add-assertion (init-len-v v))) vs))


(solver-init)
(check-sat)
(solver-push)
(init-len '((S . X_1)))
(check-sat)
(solver-pop)
(check-sat)


(defun init-consts (vs cs)
  (if (or (endp vs) (endp cs))
        nil
      (mapcar (lambda (v) (add-assertion (init-consts-v v cs))) vs)))

(solver-init)
(check-sat)
(solver-push)
(init-consts '((S . X)) '(#\1 #\2 #\3))
(check-sat)
(solver-pop)
(check-sat)

(defun init-flips (vs cs)
  (if (or (endp vs) (endp cs))
        nil
    (mapcar (lambda (v) (add-assertion (init-flips-v v cs))) vs)))


(init-flips-v '(S . X)  '(#\1 #\2 #\3))

(solver-init)
(check-sat)
(solver-push)
(init-len '((S . X)))
(init-flips '((S . X)) '(#\1))
(check-sat)
(solver-pop)
(check-sat)

;;-----------------------------------------------------------
;; Transition System
;;-----------------------------------------------------------

(defvar unsatf nil)
(defvar sat 'sat)


(solver-init)

(defmacro query-and-decide (stmt funcall)
  `(progn (solver-push)
	  (add-assertion ,stmt)
	  (let ((res ,funcall))
	    (progn (solver-pop)
		   (if (eq res unsatf)
		       unsatf
		     res)))))


;; res = oneof sat unsatf
;; query-and-decide : exp -> res -> res
;; query-and-decide exp (query-and-decide exp res)
;; query-and-decide exp (query-and-decide exp (query-and-decide exp res))
;; so is a monoid basically

(defmacro satp (stmt)
  `(progn (solver-push)
	  (add-assertion ,stmt)
	  (let ((res (eq :unsat (check-sat))))
	    (solver-pop)
	    (not res))))


(defun check-islen-more (xs l)
  (not (satp `(<= ,(seqlen xs) ,l))))


(check-islen-more '(1 2 3) 2)
(check-islen-more '(1 2 3) 3)

(check-sat)
(solver-push)
(init-len '((S . X) (S . Y) (S . Z)))
(check-islen-more '((S . X) (S . Y) (S . Z)) 3)
(solver-pop)
(check-sat)



		    
(defun greater-len-prefix (xs l &optional acc)
  (cond ((endp xs) (cons (reverse acc) nil))
	((check-islen-more acc l) (cons (reverse acc) xs))
	(t (greater-len-prefix (cdr xs) l (cons (car xs) acc)))))


(greater-len-prefix '(1 2 3 4 5) 3)
(greater-len-prefix '(1 2 3 4 5) 4)

;; returns a substitution
(defun suffix-split (u v sig cs i)
  (let ((x (car (last u))))
    (if (vp x)
	(let* ((bu (butlast u))
	       (lu (seqlen bu))
	       (v1v2 (greater-len-prefix v lu))
	       (v1 (car v1v2))
	       (v2 (cdr v1v2)))
	  (if (endp v2)
	      nil
	    (let* ((yi (new-string-var i))
		   (y (car yi))
		   (i (cdr yi))
		   (zi (new-string-var i))
		   (z (car zi))
		   (i (cdr zi))
                   (sub (hons-acons x `(,y ,z) sig)))
	      (init-len `(,y ,z))
              (add-assertion (eq-len (list x) (list y z)))
	      (add-assertion (eq-len (list z) v2))
              (if *const_constraints*
                  (progn (init-consts `(,y ,z) cs)
                         (mapcar (lambda (stm) (add-assertion stm))
                                 (eq-consts (list x) (list y z) cs))
                         (mapcar (lambda (stm) (add-assertion stm))
                                 (eq-consts (list z) v2 cs)))
                nil)
              (if *flip_constraints*
                  (progn (init-flips `(,y ,z) cs)
                         (mapcar (lambda (stm) (add-assertion stm))
                                 (bind-flips2 x y z (flips cs))))
                nil)
	      sub)))
      nil)))


(check-sat)
(solver-push)
(init-len '((S . |y|) (S . |z|) (S . |v|) (S . |w|)))
(let* ((u '((S . |y|) (#\c . 1) (S . |z|) (#\d . 1) (S . |v|) (#\g . 1)
                   (S . |w|)))
       (v '((#\a . 1) (#\b . 1) (#\c . 1) (S . |y|) (S . |y|) (#\d . 1)
                   (S . |z|) (S . |z|) (S . |z|) (S . |z|) (#\g . 1) (S . |v|))))
  
  (add-assertion (eq-len u v))
  (suffix-split u v  nil '(#\a #\b #\c #\d #\g) 0))
(solver-pop)
(check-sat)

 
(check-sat)
(solver-push)
(init-len '((S . X)))
(suffix-split '(#\1 #\2 #\3 (S . X)) '(#\1 #\2 #\3 #\4 #\5) nil
              '(#\1 #\2 #\3 #\4 #\5) 0)
(solver-pop)
(check-sat)


(defun decompose-help (u v &optional au av acc)
  (cond ((or (endp u)
	     (endp v)) (cons (cons (append (reverse au) u)
				   (append (reverse av) v))
			     acc))
	((satp `(< ,(seqlen au) ,(seqlen av)))
	 (decompose-help (cdr u) v (cons (car u) au) av acc))
	((satp `(> ,(seqlen au) ,(seqlen av)))
	 (decompose-help u (cdr v) au (cons (car v) av) acc))
	((satp `(not (= ,(seqlen au) ,(seqlen av))))
	 (decompose-help (cdr u) (cdr v)
			 (cons (car u) au)
			 (cons (car v) av)
			 acc))
	(t (decompose-help (cdr u) (cdr v)
			   (list (car u))
			   (list (car v))
			   (cons (cons (reverse au) (reverse av)) acc)))))
	
(defun decompose (u v ls vs)
  (if (satp (eq-len u v))
      (progn (add-assertion (eq-len u v))
	     (decompose-help (cdr u) (cdr v) (list (car u)) (list (car v))))
       (list (cons u v))))

(check-sat)
(solver-push)
(init-len '((S . U) (S . V) (S . W) (S . X) (S . Y) (S . Z)))
(decompose '((#\1 . 5) (S . Y) (#\2 . 3))
           '(#\1 (#\1 . 3) #\1 #\2 #\2 (S . X) #\2) nil '((S . X) (S . Y)))
(solver-pop)
(solver-push)
(init-len '((S . U) (S . V) (S . W) (S . X) (S . Y) (S . Z)))
(decompose '((S . X) (S . Y) (S . Z) (S . U)(S . W) (S . V))
           '((S . Y) (S . X)  (S . U) (S . Z) (S . V) (S . W))
           nil
           '((S . Y) (S . X)  (S . U) (S . Z) (S . V) (S . W)))
(solver-pop)
(solver-push)
(init-len '((S . U) (S . V) (S . W) (S . X) (S . Y) (S . Z)))
(decompose '((S . X) (#\1 . 1) (S . Z) (S . U) (#\1 . 2) (S . V))
           '(#\1 (S . X) (S . U) (S . Z) (S . V) #\3 #\3) nil
           '((S . Y) (S . X) (S . U) (S . Z) (S . V) (S . W)))
(solver-pop)

(solver-push)
(init-len '((S . X) (S . Y)))
(decompose '((S . X) (#\1 . 1) (S . X) (#\2 . 1) (S . Y))
           '((#\2 . 1) (S . Y) (#\2 . 1) (S . Y) (S . X))
           nil
           '((S . X) (S . Y)))
(solver-pop)
(check-sat)


;;breaks this
(solver-push)
(init-len '((S . X) (S . Y)))
(decompose '((S . X) #\c #\d #\a #\b #\g #\g #\c #\d)
           '(#\a #\b (S . Y) (S . X) (S . Y))
           nil
           '((S . X) (S . Y)))
(solver-pop)
(check-sat)

;;but not this
(solver-push)
(init-len '((S . X) (S . Y)))
(decompose '((S . X) #\c #\d #\a #\b (#\g . 2) #\c #\d)
           '(#\a #\b (S . Y) (S . X) (S . Y))
           nil
           '((S . X) (S . Y)))
(solver-pop)
(check-sat)


(defun sublistp (xs ys)
  (cond ((and (endp xs)
              (endp ys)) t)
        ((equal (car xs)
                (car ys)) (sublistp (cdr xs)
                                   (cdr ys)))
        ((endp ys) nil)
        (t (sublistp xs (cdr ys)))))
  
(sublistp '(2 3) '(1 2 3 4))
(sublistp nil '(1 2 3 4))
(sublistp '(4) '(1 2 3 4))
(sublistp '(5) '(1 2 3 4))
(sublistp '(1 2 5) '(1 2 3 4))

(defun prefixp (xs zs)
  (cond ((endp xs) zs)
        ((equal (car xs)
                (car zs)) (prefixp (cdr xs) (cdr zs)))
        (t nil)))

(prefixp '(1 2) '(1 2 3))
(prefixp nil '(1 2 3))
(prefixp '(1 2) '(2 1 2 3))

(defun rewrite-help (xs zs &optional acc)
  (let ((tl (prefixp xs zs)))
    (if (equal nil tl)
        (rewrite-help xs (cdr zs) (cons (car zs) acc))
      (cons (reverse acc) tl))))
             
(rewrite-help '(1 2) '(5 4 1 2 3))

;;rewrites xs for ys in zs
;; call only when xs is in zs
(defun rewrite (xs ys zs)
  (let ((res (rewrite-help xs zs)))
    (append (car res) ys (cdr res))))
    
(rewrite '(1 2) '(3 4) '(1 1 2 3 1 2 4 5))
(rewrite '(1 2) '(3 4) '(1 3 4 3 1 2 4 5))

(defun dbg (s)
  (if *dbg*
      (progn (print s)
             (terpri)
             (force-output))
    nil))

(defun tick (s1 s2 sig q cs &optional &key (depth 0) (check t) (i 0))
  (let* ((uv (trim s1 s2))
	 (u  (sub2 sig (car uv)))
	 (v  (sub2 sig (cdr uv)))
	 (a  (car u))
	 (b  (car v)))
    ;(log:info u)
    ;(log:info v)
    ;(log:info sig)
    ;(log:info cs)
    ;(format t (write-to-string (check-sat)))
    ;(terpri)
    (add-assertion (eq-len u v))
    (if *const_constraints*
        (mapcar (lambda (stm) (add-assertion stm)) (eq-consts u v cs))
      nil)
    (if *flip_constraints*
        (progn
          ;(print (eq-flips u v cs))
          (mapcar (lambda (stm) (add-assertion stm)) (eq-flips u v cs)))
      nil)

    (if (and check
             (eq (check-sat) :unsat))
        (progn
           (dbg "LIAUNSAT")
	   ;    (terpri)
          unsatf)
      (trivia:match (list u v q)
                    
        ;; Sat
        ((list nil nil nil) (progn (dbg "SAT")
                                  `(sat . ,sig)))
        
	;; EqElim
	((list nil nil q)
         (progn (dbg "EQELIM")
                (tick (caar q) (cdar q) sig (cdr q) cs :check nil :depth (+ depth 1) :i i)))
	
	;; ConstUnsat
	((guard (list u nil _)
		(progn (dbg "CONSTUNSAT")
                       (eq (v-in u) nil)))
         unsatf)
        
	;; UnitEmpty
	((guard (list u nil q)
                (progn (dbg "UNITEMPTY")
                       (reduce (lambda (o x) (or o x)) (mapcar 'uvarp (v-in u))
                         :initial-value nil)))
         unsatf)
        
        
	;; VarEmpty
	((list u nil q)
	 (let* ((vs1 (v-in u))
                (sub (reduce (lambda (res v) (hons-acons v nil res)) vs1
                             :initial-value sig))
		(u1 (sub2 sub u))
		(q1 (sub2-all sub q)))
	   (progn (dbg "VAREMPTY")
                  (tick u1 nil sub q1 cs :depth (+ depth 1) :i i)))) 

        ;;YVars
	((guard (list u v q)
		(and (uvarp a)
		     (uvarp b)))
         (if (equal a b)
             ;; Trim (for uvars)
             (progn (dbg "TRIM")
                    (tick (cdr u) (cdr v) sig q cs :check nil :depth (+ depth 1) :i i)))
         
           ;; DiffYVars
         (let* ((sub (hons-acons a `(,b) sig))
                (u1 (sub2 sub u))
                (v1 (sub2 sub v))
                (q1 (sub2-all sub q))
		(eqc (if *const_constraints*
			 (eq-consts `(,a) `(,b) cs)
		       nil))
                (eqf (if *flip_constraints*
                         (append (eq-begin a b cs)
                                 (eq-end a b cs))
                       nil)))
           (progn (dbg "DIFFYVARS")
                  (query-and-decide (cons 'and (append eqc eqf))
                             (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))))

        ;;UnitVar
        ((guard (list u v q)
		(and (uvarp a)
		     (svarp b)))
         (progn (dbg "UNITVAR")
                      
         (or (let* ((sub (hons-acons b nil sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q)))
	       (query-and-decide `(= ,(evar b) t)
                                 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))

             (let* ((y1 (new-xvar))
                    (sub (hons-acons b `(,a ,y1) sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q))
                    (eqc (if *const_constraints*
                             (append (+seq-countc-vars-pos+ (list y1) cs)
                                     (eq-consts `(,b) `(,a ,y1) cs))
                           nil))
                    (eqf (if *flip_constraints*
                             (append (list (init-flips-v y1 cs))
                                     (list (set-empty y1 nil)
                                           (set-empty b nil))
                                     (bind-flips3 b y1 (flips cs))
                                     (eq-begin b a cs)
                                     (eq-end b y1 cs))
                           nil)))
	       (query-and-decide (cons 'and
                                       (append (+seq-len-vars-pos+ (list y1))
                                               (list (eq-len `(b) `(,a ,y1)))
                                               eqf
                                               eqc))
                                 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i))))))
         
        ;; DiffXVars
	((guard (list u v q)
		(and (svarp a)
		     (svarp b)))
	 (progn (dbg "DIFFXVARS")
         (or (let* ((sub (hons-acons a nil sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q)))
	       (query-and-decide `(= ,(evar a) t)
                                 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))

	     (let* ((sub (hons-acons b nil sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q)))
	       (query-and-decide `(= ,(evar b) t)
                                 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))

	     (let* ((sub (hons-acons a `(,b) sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q))
		    (eqc (if *const_constraints*
			     (eq-consts `(,a) `(,b) cs)
                           nil))
                    (eqf (if *flip_constraints*
			     (append (eq-var-flips a b (flips cs))
                                     (eq-begin a b cs)
                                     (eq-end a b cs))
			 nil)))
	       (query-and-decide (cons 'and
                                       (append (list (eq-len `(,a) `(,b))
                                                     `(= ,(evar a) nil)
                                                     `(= ,(evar b) nil))
                                               eqf
                                               eqc))
                                       (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))
             

	     (let* ((wi (new-string-var i))
		    (w  (car wi))
		    (i  (cdr wi))
		    (sub (hons-acons a `(,b ,w) sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q))
		    (eqc (if *const_constraints*
                             (append (+seq-countc-vars-pos+ (list w) cs)
                                     (eq-consts `(,a) `(,b ,w) cs))
                           nil))
                    (eqf (if *flip_constraints*
			     (append (list (init-flips-v w cs))
                                     (bind-flips2 a b w (flips cs))
                                     (eq-begin a b cs)
                                     (eq-end a w cs))
			 nil)))
	       (query-and-decide (cons 'and
                                       (append (+seq-len-vars-pos+ (list w))
                                               (list (eq-len `(,a) `(,b ,w)))
                                               eqf
                                               eqc))
				 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))

	     (let* ((wi (new-string-var i))
		    (w  (car wi))
		    (i  (cdr wi))
		    (sub (hons-acons b `(,a ,w) sig))
		    (u1 (sub2 sub u))
		    (v1 (sub2 sub v))
		    (q1 (sub2-all sub q))
		    (eqc (if *const_constraints*
			     (append (+seq-countc-vars-pos+ (list w) cs)
                                     (eq-consts `(,b) `(,a ,w) cs))
                           nil))
                    (eqf (if *flip_constraints*
			     (append (list (init-flips-v w cs))
                                     (bind-flips2 b a w (flips cs))
                                     (eq-begin b a cs)
                                     (eq-end b w cs))
			 nil)))
	       (query-and-decide (cons 'and
                                       (append (+seq-len-vars-pos+ (list w))
                                               (list (eq-len `(,b) `(,a ,w)))
                                               eqf
                                               eqc))
				 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))
	     )))


        ;; UnitConst
	((guard (list u v q)
		(and (uvarp a)
                     (not (vp b))))
         (let* ((sub (hons-acons a (list (cons (car b) 1)) sig))
                (u1 (sub2 sub u))
		(v1 (sub2 sub v))
		(q1 (sub2-all sub q)))
           (or (query-and-decide `(< 1 ,(seqlen (list b)))
                                 (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i))
               (query-and-decide `(= 1 ,(seqlen (list b)))
                                 (tick (cdr u1) (cdr v1) sub q1 cs :depth (+ depth 1) :i i)))))
        
	;; VarConst
	((guard (list u v q)
		(and (svarp a)
                     (not (vp b))))

         (if (and (equal (length u) 1)
                  (endp (member a v)))
             ;;VarElim
             (progn (dbg "VARELIM")
                    (tick nil nil (hons-acons a v sig) q cs :check nil :depth (+ depth 1) :i i))
           
           (let* ((qs (decompose u v ls vs)))
	   (if (not (endp (cdr qs)))
	       ;; Decompose
	       (progn (dbg "DECOMPOSE")
                      (tick nil nil sig (append qs q) cs :check t :depth (+ depth 1) :i i))
	     
	     (let* ((sub2 (suffix-split u v sig cs i)))
	       (if (not (endp sub2))
		   ;;Suffix-Split
                   (progn (dbg "LENSPLIT")
                          (tick u v sub2 q cs :depth (+ depth 1) :i (+ i 3)))

                 (if (and (consp q)
                          (sublistp u (caar q)))
                     (progn (dbg "REWRITE")
                            (tick (rewrite u v (caar q))
                                  (cdar q) sub2 (cons (cons u v) q) cs
                                  :depth (+ depth 1) :i i))
                 
                   (progn (dbg "VARCONST")
                          (or  (let* ((sub (hons-acons a nil sig))
                                      (u1 (sub2 sub u))
                                      (v1 (sub2 sub v))
                                      (q1 (sub2-all sub q)))
                                 (query-and-decide `(= ,(evar a) t)
                                                   (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))
                               
                               (let* ((li  (new-len-var i))
                                      (l   (car li))
                                      (i   (cdr li))
                                      (sub (hons-acons a (list (cons (car b) l)) sig))
                                      (u1 (sub2 sub u))
                                      (v1 (sub2 sub v))
                                      (q1 (sub2-all sub q))
                                      (eqc (if *const_constraints*
                                               (list `(= ,(nvar a (car b))
                                                         ,l))
                                             nil))
                                      (eqf (if *flip_constraints*
                                               (append (set-flips a (flips cs) 0)
                                                       `(,(set-begin a (car b) t)
                                                         ,(set-end a (car b) t)))
                                             nil)))
                                 (query-and-decide (cons 'and
                                                         (append (+len-vars-pos+ (list l))
                                                                 (list `(< ,l ,(cdr b)))
                                                                 eqf
                                                                 eqc))
                                                   (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i)))
                               
                               (let* ((sub (hons-acons a `(,b) sig))
                                      (u1 (sub2 sub u))
                                      (v1 (sub2 sub v))
                                      (q1 (sub2-all sub q))
                                      (eqc (if *const_constraints*
                                               (eq-consts `(,a) `(,b) cs)
                                             nil))
                                      (eqf (if *flip_constraints*
                                               (append (set-flips a (flips cs) 0)
                                                       `(,(set-begin a (car b) t)
                                                         ,(set-end a (car b) t)))
                                             nil)))
                                 (query-and-decide (cons 'and
                                                         (append (list (eq-len `(,a) `(,b)))
                                                                 eqf
                                                                 eqc))
                                                   (tick (cdr u1) (cdr v1) sub q1 cs :depth (+ depth 1) :i i)))                      
                               
                               
                               (let* ((wi (new-string-var i))
                                      (w  (car wi))
                                      (i  (cdr wi))
                                      (sub (hons-acons a `(,b ,w) sig))
                                      (u1 (sub2 sub u))
                                      (v1 (sub2 sub v))
                                      (q1 (sub2-all sub q))
                                      (eqc (if *const_constraints*
                                               (append (+seq-countc-vars-pos+ `(,w) cs)
                                                       (eq-consts `(,a) `(,b ,w) cs))
                                             nil))
                                      (eqf (if *flip_constraints*
                                               (append (list (init-flips-v w cs))
                                                       `(,(set-begin a (car b) t))
                                                       (list `(implies
                                                               (not ,(evar w))
                                                               ,(cons 'and (eq-end a w cs))))
                                                       (bind-flips a w (flips cs)))
                                             nil)))
                                 (query-and-decide (cons 'and
                                                         (append (+seq-len-vars-pos+ `(,w))
                                                                 eqc
                                                                 eqf
                                                                 (list (eq-len `(,a) `(,b ,w)))))
                                                   (tick u1 v1 sub q1 cs :depth (+ depth 1) :i i))))))))))))
         
        ;; SimConst
        ((guard (list u v q)
                (and (not (vp a))
                     (not (vp b))
                     (eq (car a) (car b))))
         
         (progn (dbg "SIMCONST")            
                (or  (query-and-decide `(< ,(cdr a) ,(cdr b))
                                       (tick (cdr u)
                                             (cons (cons (car a) (list '- (cdr b) (cdr a)))
                                                   (cdr v)) sig q cs :depth (+ depth 1) :i i))              
                     
                     (query-and-decide `(= ,(cdr a) ,(cdr b))
                                       (tick (cdr u) (cdr v) sig q cs :depth (+ depth 1) :i i))
                     
                     
                     (query-and-decide `(> ,(cdr a) ,(cdr b))
                                       (tick (cons (cons (car a) (list '- (cdr a) (cdr b)))
                                                   (cdr u)) (cdr v) sig q cs :depth (+ depth 1) :i i)))))        
        
        ;; DiffConst
	((guard (list _ _ _)
		(and (not (vp a))
		     (not (vp b))))
         (progn (dbg "DIFFCONST")
                unsatf))
        
	;; Switch (for symmetry of U,V)
	((list u v q) (progn (dbg "SYMMETRY")
                      (tick v u sig q cs :depth (+ depth 1) :i i)))
        ))))


(defun join_seqs (qs)
  (cons nil (flatten
             (loop for eq in qs
                   collect (append (car eq) '(nil) (cdr eq) '(nil))))))

(join_seqs '(((#\1 #\2) #\3 #\4) ((#\5 #\6) #\7 #\8)))

(defun try (ls dict)
  (let* ((trp (car ls))
         (a (first trp))
         (b (second trp))
         (c (third trp))
         (res (hons-get b dict))
         (pr (cdr res)))
  (cond ((endp ls) dict)
        ((equal res nil) (let ((x (let ((left (hons-get a dict)))
                                    (if (and (or (equal left nil)
                                                 (equal (caddr left) b))
                                             (svarp a))
                                        a
                                      nil)))
                               (y (let ((right (hons-get c dict)))
                                    (if (and (or (equal right nil)
                                                 (equal (cadr right) b))
                                             (svarp c))
                                        c
                                      nil))))
                           (try (cdr ls) (hons-acons b (list x y) dict))))
        ((and (not (equal (first pr) a))
              (not (equal (second pr) c))) (try (cdr ls) (hons-acons b (list nil nil) dict)))
        ((not (equal (first pr) a)) (try (cdr ls) (hons-acons b (list nil c) dict)))
        ((not (equal (second pr) c)) (try (cdr ls) (hons-acons b (list a nil) dict)))
        (t (try (cdr ls) dict)))))

(try '((1 2 3) (1 2 4)) nil)

(try '((1 2 3 4 5) (1 2 3 4 5)) nil)
                    
(defun fuse_vars (qs vs)
  (let* ((ls (cons nil (join_seqs qs)))
         (as (append '(nil nil) ls))
         (bs (append '(nil) ls '(nil)))
         (cs (append ls '(nil nil)))
         (ts (mapcar #'list as bs cs))
         (trp (remove-if-not (lambda (x)
                              (svarp (second x)))
                            ts))
         (res (try trp nil)))
    (rem-dups*
     (remove nil (loop for v in vs
                       collect (second (cdr (hons-get v res))))))))


(fuse_vars 
 '((((S . X) (S . Y)) . ((S . X) (S . Y))))
 '((S . X) (S . Y) (S . V) (S . U) (S . W) (S . Z)))



(fuse_vars 
 '((((S . X) #\3 (S . Y) #\3 (S . Z) (S . V) (S . Y) #\3 (S . Y) #\1) . ((S . Y) #\1 #\3 (S . W) #\1 (S . Z) (S . V) #\2 (S . U) (S . X))))
 '((S . X) (S . Y) (S . V) (S . U) (S . W) (S . Z)))


(fuse_vars '(((#\| #\e #\~ (S . |var0|) (S . |var1|) (S . |var2|) (S . |var3|) (S . |var4|)
   (S . |var5|) (S . |var6|) (S . |var7|) (S . |var8|) (S . |var9|)
   (S . |var10|) (S . |var11|) (S . |var12|) (S . |var13|) (S . |var14|)
   (S . |var15|))
  (S . |var0|) (S . |var1|) (S . |var2|) (S . |var3|) (S . |var4|) (S . |var5|)
  (S . |var6|) (S . |var7|) (S . |var8|) (S . |var9|) (S . |var10|)
  (S . |var11|) (S . |var12|) (S . |var13|) (S . |var14|) (S . |var15|) #\' #\J
  #\2))
           '((S . |var0|) (S . |var1|) (S . |var2|) (S . |var3|) (S . |var4|)
   (S . |var5|) (S . |var6|) (S . |var7|) (S . |var8|) (S . |var9|)
   (S . |var10|) (S . |var11|) (S . |var12|) (S . |var13|) (S . |var14|)
   (S . |var15|)))


(fuse_vars '((((S . X) #\3 (S . Y) #\3 (S . V) (S . Y) #\3 (S . Y) #\1) . ((S . Y) #\1 #\3 (S . W) #\1 (S . V) #\2 (S . U) (S . X))))
           '((S . X) (S . Y) (S . V) (S . U) (S . W) (S . Z)))


(defun seqsolve (q cs vs)
  (solver-init)
  (let* ((q1  (compress-set q)))
    (solver-push)
    (init-len vs)
    (init-consts vs cs)
    (let ((res (tick nil nil nil q1 cs)))
      (solver-pop) 
      (if (eq res unsatf)
	  unsatf
	res))))


; (setq *const_constraints* t)
; (time (seqsolve '((((S . X) #\3 (S . Y) #\3 (S . Z) (S . V) (S . Y) #\3 (S . Y) #\1) . ((S . Y) #\1 #\3 (S . W) #\1 (S . Z) (S . V) #\2 (S . U) (S . X)))) '(#\1 #\2 #\3) '((S . X) (S . Y) (S . V) (S . U) (S . W) (S . Z))))
;(seqsolve '((((S . X)) . ((S . Y) #\2 (S . Z)))) nil '((S . X) (S . Y) (S . Z)))
; (seqsolve '((((S . X)) . (#\1 #\1))) '(#\1) '((S . X)))
; (seqsolve '((((S . X) #\1 #\1) . ((S . X) #\1 #\1))) '(#\1) '((S . X)))
;(seqsolve '((((S . X) #\3 (S . Y) #\3 (S . V) (S . Y) #\3 (S . Y) #\1) . ((S . Y) #\1 #\3 (S . W) #\1 (S . V) #\2 (S . U) (S . X)))) '(#\1 #\2 #\3) '((S . X) (S . Y) (S . V) (S . U) (S . W) (S . Z)))


; Flip specific example, depends on flip constraint enabling
;(setq *flip_constraints* nil)
;(seqsolve '(((#\a (S . X) #\c (S . Y) #\b) . ((S . Y) #\c #\b #\a (S . X)))) '(#\c #\b #\a) '((S . X) (S . Y)))



(defun vareqelim (qs &optional acc sub)
  (let* ((eq1 (car qs))
         (u (car eq1))
         (v (cdr eq1)))
    (cond ((endp qs) (cons acc sub))
          ((equal u v) (vareqelim (cdr qs) acc sub))
          ((and (endp (cdr u))
                (vp (car u))) (let* ((sub2 (hons-acons (car u) v sub))
                                     (qs2  (sub2-all sub2 (cdr qs)))
                                     (acc2 (sub2-all sub2 acc)))
                                (vareqelim qs2 acc2 sub2)))
          ((and (endp (cdr v))
                (vp (car v))) (let* ((sub2 (hons-acons (car v) u sub))
                                     (qs2  (sub2-all sub2 (cdr qs)))
                                     (acc2 (sub2-all sub2 acc)))
                                (vareqelim qs2 acc2 sub2)))
          (t (vareqelim (cdr qs) (cons (cons u v) acc) sub)))))



(mapcar 'z3->solver (mapcar 'car '((|L-var6| 77) (|E-var0| NIL) (|E-var2| NIL) (|E-var1| NIL) (|L-var1| 1)
 (|L-var7| 20) (|L-var4| 25) (|L-var8| 100) (|E-var4| NIL) (|E-var7| NIL)
 (|E-var5| NIL) (|E-var9| NIL) (|L-var5| 3) (|L-var0| 33) (|L-var3| 50)
 (|L-var9| 7) (|E-var8| NIL) (|L-var2| 49) (|E-var3| NIL) (|E-var6| NIL))))

(defun decompose-all (qs ls vs &optional (acc nil))
  (if (endp qs) acc
    (let ((qs2 (decompose (caar qs) (cdar qs) ls vs)))
      (decompose-all (cdr qs) ls vs (append qs2 acc)))))

;;breaks this
(solver-push)
(init-len '((S . X) (S . Y)))
(decompose-all '((((S . X) #\c #\d #\a #\b #\g #\g #\c #\d) . (#\a #\b (S . Y) (S . X) (S . Y))))
           nil '((S . X) (S . Y)))
(solver-pop)
(check-sat)


(defun collect-all-constraints (qs cs)
  (if (endp qs)
      nil
    (let ((u (caar qs))
          (v (cdar qs)))
      (append (collect-all-constraints (cdr qs))
              (list (eq-len u v))
              (eq-consts u v cs)
              (eq-flips u v cs)))))



(defun main_seqsolve (qs3 lia-eqs cs vs ls i)
  (solver-push)
  (mapcar 'add-assertion lia-eqs)
  (if *len_constraints*
      (progn (init-len vs)
             (mapcar (lambda (eq) (add-assertion (eq-len (car eq) (cdr eq)))) qs3))
    nil)
  (let* ((res2 (vareqelim qs3))
         (qs2  (car res2))
         (sub2 (cdr res2))
         (evs (if (endp lia-eqs)
                  (fuse_vars qs2 vs)
                nil))
         (sub (reduce (lambda (res v) (progn (add-assertion `(= ,(evar v) t))
                                             (hons-acons v nil res)))
                      evs
                      :initial-value sub2))
         (qs3 (if (< (len qs2) 5)
                  (decompose-all qs2 ls vs)
                qs2))
         (qs4 (sub2-all sub qs3))
         (qs  (compress-set qs4))
         (vs3 (reduce (lambda (res eq) (append res
                                               (append (v-in (car eq))
                                                       (v-in (cdr eq)))))
                      qs :initial-value nil))
         (vs2 (rem-dups* vs3)))
    ;;NOTE : we are checking num vars only AFTER doing VAREQELIM, SO MUST BE LOW, covers several cases which have far more vars, but after fuse vars, end up with small number of vars.
    (setq *const_constraints* (< (len vs2) 20))
    (setq *flip_constraints* (< (len vs2) 10))
    (if *const_constraints*
        (init-consts vs2 cs)
      nil)
    (if *flip_constraints*
        (progn (init-flips vs2 cs)
               (if (< (len qs2) 5)
                   (mapcar (lambda (stm) (add-assertion stm)) (collect-all-constraints qs4 cs))
                 nil))
      nil)
    (let ((res (tick nil nil sub qs cs :i (+ i 1))))
      (solver-pop) 
      (if (eq res unsatf)
          unsatf
        res))))

  
;;-----------------------------------------------------------
;; Read file, send to solver
;;-----------------------------------------------------------

(z3::solver-init)

(defun main (f)
  (solver-init)
  (setq *dbg* nil)
  (trivia:match (parse f 0)
                ((list i qs lia-eqs cs xs ys ls)
                 (let* ((vs (rem-dups* (append xs ys)))
                        (ls (rem-dups* ls))
                        (cs (if (and (endp cs)
                                     (not (endp ys)))
                                '(#\a) ;; Choose(Gamma)
                              (rem-dups* cs))))
                   (main_seqsolve qs lia-eqs cs vs ls i)))))



(main_seqsolve '((((S . |e|)) (S . |e|)))
               '((= 1 (+ (L . |e|))))
               '((S . |e|))
               '()
               nil
               2)


;(main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/abcyydzzzzgvEQyczdvgw.smt2")

;(main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/xcyczvycyaEQyacwazvbux.smt2")

;(main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/unitEQvar.smt2")

;(time (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/overlaps-big/overlaps-big-00016-0.smt25"))

;(time (main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/abcyydzzzzgvEQyczdvgw.smt2"))

;(time (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/overlaps-small/overlaps-small-00001-1.smt25"))

;(main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/concats-balanced/concats-balanced-00001-0.smt25")

;; (main "/Users/ankitku/dev/monoid-solver/evaluation/xaxbybybyxEQbybyxxaxby.smt2")
;; (main "/Users/ankitku/dev/monoid-solver/evaluation/xcdabggcdEQabyxy.smt2")
;; (main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/unsat/xaxbyEQbybyx.smt2")


(defun main_cl (args)
  (if (equal (car args)
             "--version")
      (print "SeqSolve 1.0")
    (progn
      (solver-init)
      (setq *dbg* nil)
      (trivia:match (parse (car args) 0)
                    ((list i qs lia-eqs cs xs ys ls)
                     (let* ((cs (rem-dups* cs))
                            (vs (rem-dups* (append xs ys)))
                            (ls (rem-dups* ls)))
                       (let ((res (main_seqsolve qs lia-eqs cs vs ls i)))
                         (if (equal res nil)
                             (format t "~%unsat~%")
                           (format t "~%sat~%"))
                         (terpri)))))))
  (sb-ext:exit))


;(time (main_cl '("/Users/ankitku/dev/monoid-solver/evaluation/filtered/sat/withlen/3464.corecstrs.readable.smt2")))

;(time (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/concats-balanced/concats-balanced-00007-0.smt25"))

(defun solve_dir (args)
  (solver-init)
  (cl-fad:walk-directory (car args)
                         (lambda (name)
                           (let ((f (namestring name)))
                           (if (and (search "smt2" f)
                                    (not (search "smt20" f)))
                               (progn
                                 ;;uncomment to print name of file along with result
                                 (format t "~A~%" name)
                                 (solver-init)
                                 (format t "~A~%" (car (main f))))
                             nil)))
                         :directories t))





;;some profiling-------------------------------------------------------
;(load "metering.cl")

;(mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/concats-balanced/concats-balanced-00010-9.smt25"))

; (mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/lengths-long/lengths-long-04901-1.smt25"))

; (mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/concats-big/concats-big-01161-1.smt25"))

; (mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/overlaps-big/overlaps-big-00136-1.smt25"))

; (mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/filtered/sat/withlen/3464.corecstrs.readable.smt2"))

; (mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/unsat/xaxbybvbvwEQbybyxvavabw.smt2"))

; (mon:with-monitoring (main check-islen-more z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/unsat/xaxbyEQbybyx.smt2"))

; (mon:with-monitoring (main decompose greater-len-prefix check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/xcyczvycyaEQyacwazvbux.smt2"))

; (mon:with-monitoring (main z3:check-sat) () (main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/abcyydzzzzgvEQyczdvgw.smt2"))

; (time (solve_dir '("/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/overlaps-small/")))

; (time (solve_dir '("/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/")))
; (solve_dir '("/Users/ankitku/dev/monoid-solver/evaluation/filtered/"))


(defun smt-or-smt25p (fl)
  (let ((f (namestring fl)))
    (and (search "smt2" f)
         (not (search "smt20" f)))))

;; runs seqsolve on all examples in a dir, then creates json output for plot
(defun eval_dir1 (benchmark i)
  (let* ((bname (car (reverse (cl-ppcre:split "\/" benchmark))))
         (js2 (jsown:new-js
               ("benchmark" bname)
               ("prog_args" "")
               ("program" "seqsolve-srv")
               ("prog_alias" "ssls")))
         (js1 (jsown:new-js
               ("preamble" js2)))
         (js3 (jsown:new-js)))
    (setq *runs* nil)
    (terpri)
    (cl-fad:walk-directory
     benchmark
     (lambda (fl)
       (let* ((f (namestring fl))
              (fname (enough-namestring fl))
              (x (solver-init))
              (rt1 (get-internal-run-time))
              (res (car (main f)))
              (rt2 (get-internal-run-time)))
         (force-output)
         (if (equal res nil)
             (format t "~c[31mu~c[0m" #\ESC #\ESC)
           (format t "~c[32ms~c[0m" #\ESC #\ESC))
         (force-output)
         (setf *runs*
               (cons (cons
                      (float (/ (- rt2 rt1)
                                internal-time-units-per-second))
                      (cl:file-namestring fname)) *runs*))))
     :test 'smt-or-smt25p
     :directories nil)
    (let* ((rs (reduce (lambda (js tf) (jsown:extend-js
                                        js
                                        ((cdr tf) (jsown:new-js
                                                   ("rtime" (car tf))
                                                   ("status" t)
                                                   ("res" "solved")))))
                       *runs*
                       :initial-value js3))
           (resjson (jsown:extend-js
                     js1
                     ("stats" rs)))
           (jsonfile (format nil "~a-seqsolve-srv-~a.json" bname i)))
        (with-open-file (stream jsonfile
                                :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
          (format stream (jsown:to-json resjson))))))


(defun eval_dir (args)
  (let* ((bmark (first args))
	 (iter (parse-integer (second args))))
    (loop for i from 1 to iter 
          do (eval_dir1 bmark i))
    (sb-ext:exit)))

;(eval_dir '("/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/overlaps-small/")) 
;(eval_dir '("/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/"))

;(time (eval_dir '("/Users/ankitku/dev/monoid-solver/evaluation/stringfuzz-gen/")))

; (eval_dir '("/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/"))

;(main "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/abcyydzzivvgwwhxxEQyczdviwgxhu.smt2")
