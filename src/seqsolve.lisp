(in-package "ACL2S")
(include-book "higher-order")
(set-slow-alist-action nil)

;;-----------------------------------------------------------
;; Sequences
;;-----------------------------------------------------------

;; a is alphabet
(defdata a character)
(defdata svar (cons 'S symbol))
(defdata uvar (cons 'U symbol))
(defdata lvar (cons 'L symbol))
(defdata nvar (list 'N symbol a))
(defdata fvar (list 'F symbol a a))

(defdata evar (cons 'E symbol))
(defdata begin_var (list 'b symbol a))
(defdata end_var (list 'd symbol a))
(defdata bvar (oneof evar begin_var end_var))

(check= (nvarp '(N X_2 #\1)) t)
(check= (evarp '(E . X)) t)
(check= (fvarp '(F X #\a #\b)) t)

(defdata v (oneof svar uvar))
(defdata lov (listof v))
(defdata loa (listof a))

(defdata op (oneof '+ '- '*))
(defdata bop (oneof 'if 'or 'and 'not 'implies '= '<= '>= ))
(defdata comp (oneof '= '<= '>= '< '>))

(defdata lexp (oneof nat
		     lvar
		     evar
		     (list op lexp lexp)))

(defdata
  (exp (oneof nat
	      lvar
	      nvar
	      evar
	      fvar
	      (list op exp exp)
	      bexp))
  (bexp (oneof boolean
	       bvar
	       begin_var
	       end_var
	       (list 'not bexp)
	       (list comp exp exp)
	       (list 'implies exp exp)
	       (list 'if bexp exp exp)
	       (cons 'or lobexp)
	       (cons 'and lobexp)))
  (lobexp (listof bexp)))


(check= (lexpp '(+ 1 (L . X))) t)
(check= (lexpp '(+ 1 (E . X))) t)
(check= (lexpp '(+ 1 9)) t)
(check= (lexpp '(/ 9 (L . X))) ())

(check= (expp '(= (L . X) (L . X))) t)
(check= (expp '(= (L . X) 0)) t)
(check= (expp '(implies (E . X) (= (L . X) 0))) t)
(check= (bexpp '(and (implies (E . X) (= (L . X) 0))
		     (E . X)
		     (E . X)
		     (E . X)
		     (E . X))) t)


(defdata-subtype lexp exp)
(defdata-subtype bexp exp)


(defdata block (cons a lexp))

(check= (blockp '(#\1 . 1)) t)
(check= (blockp '(#\1 . (L . x))) t)
(check= (blockp '(#\1 . (+ 1 (L . X)))) t)
(check= (blockp '(#\1 . (1))) ())
(check= (blockp '(#\3 . (1 1 (L . X)))) ())

(defdata elem (oneof a svar uvar block))
(defdata seq (listof elem))

(check= (seqp ()) t)
(check= (seqp '(#\1 #\2 (#\1 . 1) (S . X))) t)
(check= (seqp '( #\1 (U . E) (#\2 . (- 5 (- (+ (L . Y) (L . Z)) 3))))) t)

(defdata sigma (alistof v seq))
(defdata eq (cons seq seq))
(defdata qs (listof eq))

(defthm qs-seqs
  (=> (qsp qs)
      (and (seqp (caar qs))
	   (seqp (cdar qs)))))

(create-filter* (lambda (x) (vp x)) seqp)
(definecd v-in (seq :seq) :lov
  (filter* (lambda (x) (vp x)) seq))

(check= (v-in '(#\1 #\2 #\3 (S . X))) '((S . X)))

(definecd trim-left (u :seq v :seq) :eq
  (cond ((or (endp u) (endp v)) (cons u v))
	((equal (car u) (car v)) (trim-left (cdr u) (cdr v)))
	(t (cons u v))))

(check= (trim-left '(#\1 #\1 #\2 #\3 #\4 #\5 #\6)
		   '(#\1 #\1 #\2 #\7 #\5 #\6))
	'((#\3 #\4 #\5 #\6) #\7 #\5 #\6))

(definecd trim (u :seq v :seq) :eq
  (let* ((tluv (trim-left u v))
	 (tlu (car tluv))
	 (tlv (cdr tluv))
	 (tuv (trim-left (reverse tlu)
			 (reverse tlv)))
	 (tu (reverse (car tuv)))
	 (tv (reverse (cdr tuv))))
    (cons tu tv)))

(check= (trim '(#\1 #\1 #\2 #\3 #\4 #\5 #\6) '(#\1 #\1 #\2 #\7 #\5 #\6))
	'((#\3 #\4) #\7))

(create-filter* (lambda (x) (not (equal (car x) (cdr x)))) qsp)
(definecd eqelim (qs :qs) :qs
  (filter* (lambda (x) (not (equal (car x) (cdr x)))) qs))

(check= (eqelim '(((#\a) . (#\b)) (() . ())))
	'(((#\a) . (#\b))))

;;-----------------------------------------------------------
;; Substitution
;;-----------------------------------------------------------

(defthm sigma-res
  (=> (sigmap sig)
      (seqp (cdr (hons-assoc-equal v sig)))))

;; a key not in substitution returns nil. a key in substitution returns a pair
(check= (hons-get '(S . X) (hons-acons '(S . Y) '(#\1 #\2) (hons-acons '(S . X) () 20))) '((S . X)))
(check= (hons-get '(S . Y) (hons-acons '(S . Y) '(#\1 #\2) (hons-acons
'(S . X) () 20))) '((S . Y) . (#\1 #\2)))
(check= (hons-get '(S . Z) (hons-acons '(S . Y) '(#\1 #\2) (hons-acons '(S
. X) () 20))) ())

(definecd sub2 (sig :sigma seq :seq) :seq
  (if (endp seq)
      ()
  (let ((res (if (vp (car seq))
		 (hons-get (car seq) sig)
	       ())))
    (if (endp res)
	(cons (car seq) (sub2 sig (cdr seq)))
      (append (cdr res) (sub2 sig (cdr seq)))))))


(create-map* (lambda (eq1 sub)
	       (cons (sub2 sub (car eq1))
		     (sub2 sub (cdr eq1)))) qsp qsp
		     (:fixed-vars ((sigmap sub)))
		     (:name sub-map))
		     
(definecd sub2-all (sub :sigma qs :qs) :qs
  (map* sub-map qs sub))


;; sub2 checks
(check= (sub2 '(((S . Y) #\1 #\2)) '((S . Y) #\3 #\4 (S . Y))) '(#\1 #\2 #\3 #\4 #\1 #\2))
(check= (sub2 '(((S . X))) '((S . X) (S . X) (S . X) (S . Y) (S . X))) '((S . Y)))
(check= (sub2 '(((S . X))) '((S . X) (#\1 . 1)  (#\2 . 2) (S . X))) '((#\1 . 1) (#\2 . 2)))

(check= (sub2 '(((S . X) #\1 (U . Y) #\2 (#\3 . (+ (L . X1) 5)))) '((S . X) (S . X) (S . X) (S . Y) (S . X))) '(#\1 (U . Y)
   #\2 (#\3 + (L . X1) 5)
   #\1 (U . Y)
   #\2 (#\3 + (L . X1) 5)
   #\1 (U . Y)
   #\2 (#\3 + (L . X1) 5)
   (S . Y)
   #\1 (U . Y)
   #\2 (#\3 + (L . X1) 5)))


(check= (sub2 '(((S . X) (#\3 . (+ (+ (L . X1) (L . L1)) 5)))) '((S . X) (S . X) (S . X) (S . Y) (S . X)))
'((#\3 + (+ (L . X1) (L . L1)) 5)
 (#\3 + (+ (L . X1) (L . L1)) 5)
 (#\3 + (+ (L . X1) (L . L1)) 5)
 (S . Y)
 (#\3 + (+ (L . X1) (L . L1)) 5)))



(check= (sub2-all '(((S . X) #\1 #\2) ((S . Y) #\3 #\4))
			'((((S . X) (S . Y) (S . X)) . ((S . Y) (S . X) (S . X) (S . X) (S . Y) (S . X) (S . X)))))
	'(((#\1 #\2 #\3 #\4 #\1 #\2) #\3 #\4 #\1 #\2 #\1 #\2 #\1 #\2 #\3 #\4 #\1 #\2 #\1 #\2)))

(defdata qsig (cons qs sigma))
(defdata-subtype qsig cons)

(in-theory (disable qsigp qsp elemp seqp vp blockp ap))

(defthm sub-len-qs
  (=> (and (qsp qs)
	   (sigmap sub))
      (= (len (sub2-all sub qs))
	 (len qs)))
  :hints (("goal" :in-theory (enable sub2-all))))

(definecd vareqelim (qs :qs acc :qs sub :sigma) :qsig
  :skip-tests t
  :function-contract-strictp t
  :body-contracts-strictp t
  (declare (xargs :consider-only-ccms ((len qs))))
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

;-----------------------
; Aux functions
;-----------------------
(defdata maybe-a (oneof () a))
(definecd compress-help (seq :seq cur :maybe-a n :lexp) :seq
  (let ((c (car seq)))
    (cond ((and (endp seq) (equal cur ())) ())
	  ((endp seq) (list (cons cur n)))
	  ((and (vp c) (equal cur ())) (cons c (compress-help (cdr seq) () 0)))
	  ((vp c) (cons (cons cur n) (cons c (compress-help (cdr seq) () 0))))
	  ((and (blockp c)
		(equal (car c) cur)
		(not (equal n 0))) (compress-help (cdr seq) cur `(+ ,n ,(cdr c))))
	  ((and (blockp c)
		(equal cur ())) (compress-help (cdr seq) (car c) (cdr c)))
	  ((blockp c) (cons (cons cur n) (compress-help (cdr seq) (car c) (cdr c))))
	  ((equal c cur) (compress-help (cdr seq) c `(+ ,n ,1)))
	  ((equal cur ()) (compress-help (cdr seq) c 1))
	  (t (cons (cons cur n) (compress-help (cdr seq) c 1))))))

(definecd compress (seq :seq) :seq
  (compress-help seq () 0))

(check= (compress '((S . X_1))) '((S . X_1)))
(check=  (compress `(#\1 (S . X) (S . Y_1) #\5 (#\5 . (L . X)) (S . Z) #\2 #\2 #\2))
	 '((#\1 . 1)
	   (S . X)
	   (S . Y_1)
	   (#\5 + 1 (L . X))
	   (S . Z)
	   (#\2 + (+ 1 1) 1)))


(definecd compress-eq (eq :eq) :eq
  `(,(compress (car eq)) . ,(compress (cdr eq))))

(create-map* compress-eq qsp qsp)
(definec compress-set (qs :qs) :qs
  (map* compress-eq qs))

(check= (compress-set '((((S . X)) . (#\5 (S . Y) #\2 #\2 (S . Z))))) '((((S . X))
  (#\5 . 1)
  (S . Y)
  (#\2 + 1 1)
  (S . Z))))


(definecd lvar (v :v) :lvar
  (cons 'L (cdr v)))

(check= (lvar '(S . X)) '(L . X))


(definecd nvar (v :v c :a) :nvar
  (list 'N (cdr v) c))

(check= (nvar '(S . X) #\1) '(N  X  #\1))

(defthm nvar-exp
     (implies (and (vp x) (ap a))
	      (expp (nvar x a))))

(definecd evar (v :v) :evar
  (cons 'E (cdr v)))

(definecd fvar (v :v a :a b :a) :fvar
  (list 'f (cdr v) a b))

(definecd begin-var (v :v c :a) :begin_var
  (list 'b (cdr v) c))

(definecd end-var (v :v c :a) :end_var
  (list 'd (cdr v) c))


;;--------- len counting --------- 
(definecd seqlen (seq :seq) :lexp
  (let ((c (car seq)))
  (cond ((endp seq) 0)
	((svarp c) `(+ ,(lvar c) ,(seqlen (cdr seq))))
	((uvarp c) `(+ 1 ,(seqlen (cdr seq))))
	((blockp c)`(+ ,(cdr c) ,(seqlen (cdr seq))))
        (t `(+ 1 ,(seqlen (cdr seq)))))))

;;--------- constant counting --------- 
(definecd countc (seq :seq a :a) :exp
  (let ((c (car seq)))
  (cond ((endp seq) 0)
	((vp c) `(+ ,(nvar c a) ,(countc (cdr seq) a)))
	((and (blockp c)
	      (equal (car c) a)) `(+ ,(cdr c) ,(countc (cdr seq) a)))
	((blockp c) (countc (cdr seq) a))
	((equal c a) `(+ 1 ,(countc (cdr seq) a)))
        (t (countc (cdr seq) a)))))


;;--------- flip counting --------- 
(definecd b->i (b :bexp) :bexp
  `(if ,b 1 0))

(definecd b-mul (p :bexp q :bexp) :bexp
  `(if ,p (if ,q 1 0) 0))

(definecd b-imp (b :bexp exp :exp) :bexp
  `(if ,b ,exp 0))

(definecd b-or (p :bexp q :bexp) :bexp
  `(or ,p ,q))

(defdata maybe-v (oneof () v))
(definecd count-flips-from (u :seq x :maybe-v empty :lov b :a) :exp
  :timeout 300
  (let ((c (car u)))
    (cond ((endp u) 0)
	  ((or (and (blockp c)
		    (equal (car c) b))
	       (and (ap c)
		    (equal c b))) 1)
	  ((in c empty) (count-flips-from (cdr u) x empty b))
	  ((equal c x) (b->i (begin-var c b)))
	  ((vp c) `(+ ,(b->i (begin-var c b))
		      ,(b-imp (evar c)
			      (count-flips-from (cdr u) x (cons c empty) b))))
	  (t 0))))

(definecd count-flip (u :seq a :a b :a) :exp
  (let ((c (car u)))
    (cond ((endp u) 0)
	  ((or (and (blockp c)
		    (equal (car c) a))
	       (and (ap c)
		    (equal c a))) `(+ ,(count-flips-from (cdr u) () () b)
				      ,(count-flip (cdr u) a b)))
	  ((vp c) (list '+ (list '+ (fvar c a b)
				 (b-imp (end-var c a)
					(count-flips-from (cdr u) c () b)))
			(count-flip (cdr u) a b)))
	  (t (count-flip (cdr u) a b)))))



(check=
 (count-flip '((S . |x|) (#\c . 1) (#\d . 1) (#\a . 1) (#\b . 1) (#\g + 1 1) (#\c . 1)
	      (#\d . 1)) #\g #\c)
 '(+ (+ (F |x| #\g #\c) (IF (D |x| #\g) 1 0))
    (+ 1 0)))


(check= (count-flip '((#\a . 1) (S . X) (S . Y) (#\b . 1)) #\a #\b)
	'(+ (+ (IF (B X #\b) 1 0)
	       (IF (E . X)
		   (+ (IF (B Y #\b) 1 0) (IF (E . Y) 1 0))
		   0))
	    (+ (+ (F X #\a #\b)
		  (IF (D X #\a)
		      (+ (IF (B Y #\b) 1 0) (IF (E . Y) 1 0))
		      0))
	       (+ (+ (F Y #\a #\b) (IF (D Y #\a) 1 0))
		  0))))



;assign empty substitution to variables in vs
(definecd set-empty-vs (vs :lov) :sigma
  (cond ((endp vs) ())
	(t (hons-acons (car vs)
		       () (set-empty-vs (cdr vs))))))

(check= (set-empty-vs '((S . X) (S . Y) (S . Z)))
	'(((S . X)) ((S . Y)) ((S . Z))))


(definecd rem-dups-t (l :tl  acc :tl) :tl
  (cond ((endp l) (reverse acc))
        ((in (car l) (cdr l)) (rem-dups-t (cdr l) acc))
        (t (rem-dups-t (cdr l)
		       (cons (car l) acc)))))

(definecd rem-dups* (l :tl) :tl
  (rem-dups-t l ()))

(check= (rem-dups* '(1 1 2 2 3 3)) '(1 2 3))



(defdata svar-nat (cons svar nat))
(definecd new-string-var (idx :nat) :all
  (cons (cons 'S (gen-sym-pkg (list 'X idx) "ACL2S"))
	(1+ idx)))


(defdata lvar-nat (cons lvar nat))
(definecd new-len-var (idx :nat) :all
  (cons (cons 'L (gen-sym-pkg (list 'L idx) "ACL2S"))
	(1+ idx)))



;;-----------------------------------------------------------
;; Typing Z3 Expressions, Utility Functions and Assertions
;;-----------------------------------------------------------  


;;assertion types for Z3
(defdata z3lexp (list comp lexp lexp))
(defdata loz3lexp (listof z3lexp))
(defdata z3exp bexp)
(defdata loz3exp (listof z3exp))

(defdata vexp (oneof lvar nvar evar fvar))
(defdata lovexp (listof vexp))
(defdata liatypes (oneof :int :bool))

(defdata-subtype loz3exp lobexp)
(defdata-subtype lobexp loz3exp)

(definecd eq-len (u :seq v :seq) :z3lexp
  (list '= (seqlen u) (seqlen v)))


(definecd sum-nvars (v :v cs :loa) :exp
  (cond ((endp cs) 0)
	(t `(+ ,(nvar v (car cs)) ,(sum-nvars v (cdr cs))))))

(definecd v-sumconsts (v :v cs :loa) :z3exp
  (list '= (sum-nvars v cs) (lvar v)))
  
(check= (v-sumconsts '(S . X) '(#\1 #\2 #\3))
	'(= (+ (N X #\1) (+ (N X #\2) (+ (N X #\3) 0))) (L . X)))

(definecd eq-consts (u :seq v :seq cs :loa) :loz3exp
  (cond ((endp cs) ())
	(t (cons (list '=
		       (countc u (car cs))
		       (countc v (car cs)))
		 (eq-consts u v (cdr cs))))))


(definecd set-empty (v :v b :boolean) :z3exp
  `(= ,(evar v) ,b))

(definecd set-begin (v :v c :a b :boolean) :z3exp
  `(= ,(begin-var v c) ,b))

(definecd set-end (v :v c :a b :boolean) :z3exp
  `(= ,(end-var v c) ,b))

(definecd eq-begin (x :v y :v cs :loa) :loz3exp
  (let ((c (car cs)))
    (if (endp cs) ()
      (cons `(= ,(begin-var x c) ,(begin-var y c))
	    (eq-begin x y (cdr cs))))))

(definecd eq-end (x :v y :v cs :loa) :loz3exp
  (let ((c (car cs)))
    (if (endp cs) ()
      (cons `(= ,(end-var x c) ,(end-var y c))
	    (eq-end x y (cdr cs))))))

(definecd eq-end-begin (x :v y :v cs :loa) :loz3exp
  (let ((c (car cs)))
    (if (endp cs) ()
      (cons `(= ,(end-var x c) ,(begin-var y c))
	    (eq-end-begin x y (cdr cs))))))

(definecd eq-flip (u :seq v :seq a :a b :a) :z3exp
  `(= ,(count-flip u a b)
      ,(count-flip v a b)))

(check= (eq-flip () () #\e #\4)
	'(= 0 0))

(defdata const_pair (alistof a a))
(definecd mapcons (x :a ls :loa) :const_pair
  (if (endp ls) ()
    (cons `(,x . ,(car ls))
	  (cons `(,(car ls) . ,x)
		(mapcons x (cdr ls))))))

(definecd flips (cs :loa) :const_pair
  (if (endp cs) ()
    (append (mapcons (car cs) (cdr cs))
	(flips (cdr cs)))))

(definecd eq-flips-help (u :seq v :seq aa :const_pair) :loz3exp
  (if (endp aa) ()
    (cons (eq-flip u v (caar aa) (cdar aa))
	  (eq-flips-help u v (cdr aa)))))

(definecd eq-flips (u :seq v :seq cs :loa) :loz3exp
  (eq-flips-help u v (flips cs)))
    
(definecd set-flips (x :v aa :const_pair i :nat) :loz3exp
  (if (endp aa) ()
    (cons `(= ,(fvar x (caar aa) (cdar aa)) ,i)
	  (set-flips x (cdr aa) i))))

(definecd bind-flips (x :v y :v aa :const_pair) :loz3exp
  (let ((c (caar aa))
	(d (cdar aa)))
    (if (endp aa) ()
      (append (list `(<= (- ,(fvar x c d) 1) ,(fvar y c d))
		    `(<= ,(fvar y c d) ,(fvar x c d))
		    `(<= ,(fvar x c d) (+ 1 ,(fvar y c d))))
	      (bind-flips x y (cdr aa))))))

(definecd bind-flips2 (x :v y :v z :v aa :const_pair) :loz3exp
  (let ((c (caar aa))
	(d (cdar aa)))
    (if (endp aa) ()
      (append (list `(<= (+ ,(fvar y c d) ,(fvar z c d)) ,(fvar x c d))
		    `(<= ,(fvar x c d) (+ 1 (+ ,(fvar y c d) ,(fvar z c d)))))
	      (bind-flips2 x y z (cdr aa))))))

(definecd bind-flips3 (x :v y :v aa :const_pair) :loz3exp
  (let ((c (caar aa))
	(d (cdar aa)))
    (if (endp aa) ()
      (append (list `(<= ,(fvar y c d) ,(fvar x c d))
		    `(<= ,(fvar x c d) (+ 1 ,(fvar y c d))))
	      (bind-flips3 x y (cdr aa))))))

(definecd eq-var-flips (x :v y :v aa :const_pair) :loz3exp
  (if (endp aa) ()
    (cons `(= ,(fvar x (caar aa) (cdar aa))
	      ,(fvar y (caar aa) (cdar aa)))
	  (eq-var-flips x y (cdr aa)))))



;;-----------------------------------------------------------  

(defdata declx (oneof (cons vexp (cons liatypes declx))
		()))
(check= (declxp '((L . X) :INT (L . Y) :INT (L . Z) :INT)) t)

(defthm APP-DECLX
  (=> (and (declxp l1) (declxp l2))
      (declxp (append l1 l2))))

(defdata iv (oneof lvar nvar fvar))
(defdata ivs (listof iv))     ;integer variables in z3 expression
(defdata bv (oneof evar begin_var end_var))
(defdata bvs (listof bv))                  ;boolean variables in z3 expression
(defdata ivs-bvs (list ivs bvs))

(defthm IB1
  (=> (ivs-bvsp xs)
      (ivsp (first xs))))

(defthm IB2
  (=> (ivs-bvsp xs)
      (bvsp (second xs))))

;; gets list of (intvars boolvars) for declaration
(definecd collect-vs (e :all) :ivs-bvs
  (cond ((atom e) (list () ()))
        ((or (lvarp e)
             (nvarp e)
	     (fvarp e))  (list `(,e) ()))
        ((or (evarp e)
	     (begin_varp e)
	     (end_varp e)) (list () `(,e)))
        (t (let* ((ls1vs1 (collect-vs (car e)))
                  (ls1 (first ls1vs1))
                  (vs1 (second ls1vs1))
                  (ls2vs2 (collect-vs (cdr e)))
                  (ls2 (first ls2vs2))
                  (vs2 (second ls2vs2)))
             (list (append ls1 ls2)
                   (append vs1 vs2))))))

(check= (collect-vs '(seqlen (ACL2S::(S . X_1) #\1 #\2 #\3 (#\5 . ACL2S::(+ (L . X_1) #\3))))) '(((L . X_1)) ()))


(definecd +bool-vars-decl+ (bs :bvs) :tl
  (cond ((endp bs) ())
	(t (append (list (car bs) :bool)
		   (+bool-vars-decl+ (cdr bs))))))
  
(check= (+bool-vars-decl+ '((E . X) (E . Y) (E . Z)))
	'((E . X) :BOOL (E . Y) :BOOL (E . Z) :BOOL))

	
(definecd +int-vars-decl+ (is :ivs) :tl
  (cond ((endp is) ())
	(t (append (list (car is) :int)
		   (+int-vars-decl+ (cdr is))))))

(check= (+int-vars-decl+ '((L . X) (L . L0)))
	'((L . X) :INT (L . L0) :INT))


(defdata lolvar (listof lvar))
(definecd +len-vars-pos+ (lvs :lolvar) :loz3exp
  (cond ((endp lvs) ())
	(t (cons (list '<= 0 (car lvs))
		 (+len-vars-pos+ (cdr lvs))))))

(definecd +seq-len-vars-pos+ (vs :lov) :loz3lexp
  (cond ((endp vs) ())
	(t (cons (list '<= 0 (lvar (car vs)))
		 (+seq-len-vars-pos+ (cdr vs))))))

(check= (+seq-len-vars-pos+ '((S . X) (S . Y) (S . Z)))
	'((<= 0 (L . X)) (<= 0 (L . Y)) (<= 0 (L . Z))))

(definecd nvar-pos (v :v cs :loa) :loz3exp
  (cond ((endp cs) ())
	(t (cons (list '<= 0 (nvar v (car cs)))
		   (nvar-pos v (cdr cs))))))

(definecd +seq-countc-vars-pos+ (vs :lov cs :loa) :loz3exp
  (cond ((endp vs) ())
	(t (append (nvar-pos (car vs) cs)
		   (+seq-countc-vars-pos+ (cdr vs) cs)))))

(check= (+seq-countc-vars-pos+ '((S . X) (S . Y) (S . Z)) '(#\1 #\2))
'((<= 0 (N X #\1))
 (<= 0 (N X #\2))
 (<= 0 (N Y #\1))
 (<= 0 (N Y #\2))
 (<= 0 (N Z #\1))
 (<= 0 (N Z #\2))))


(definecd init-len-v (v :v) :z3exp
  `(and (implies ,(evar v)  (= ,(lvar v) 0))
	(implies  (= ,(lvar v) 0) ,(evar v))
	(and (implies (not ,(evar v))  (> ,(lvar v) 0))
	     (<= 0 ,(lvar v)))))


(definecd init-consts-v (v :v cs :loa) :z3exp
    (if (endp cs)
	nil
      (cons 'and
	    (cons (v-sumconsts v cs)
		  (+seq-countc-vars-pos+ (list v) cs)))))

(defdata lpa (alistof a a))

(definecd in-flips (v :v lp :lpa) :lovexp
  (if (endp lp)
      ()
    (cons (fvar v (caar lp) (cdar lp))
	  (in-flips v (cdr lp)))))
  
(definecd initflips-vab (v :v lp :lpa) :loz3exp
  (if (endp lp)
      ()
    (let* ((p (car lp))
	   (a (car p))
	   (b (cdr p))
	   (fvab (fvar v a b)))
      (append (if (uvarp v)
		  `((= 0 ,fvab)
		    (= ,(begin-var v a) ,(end-var v a)))
		`((<= 0 ,fvab)
		  (<= (* 2 ,fvab) ,(lvar v))))
	      (initflips-vab v (cdr lp))))))


(check= (initflips-vab '(S . X) '((#\1 . #\2)))
	'((<= 0 (F X #\1 #\2))
	  (<= (* 2 (F X #\1 #\2)) (L . X))))
	


(defun list-beginvars (v cs)
  (if (endp cs) ()
    (cons (b->i (begin-var v (car cs))) (list-beginvars v (cdr cs)))))

(defun list-endvars (v cs)
  (if (endp cs) ()
    (cons (b->i (end-var v (car cs))) (list-endvars v (cdr cs)))))

(defun init-flips-v (v cs)
  (let ((lp (flips cs)))
    (if (endp lp)
	t ;;returns true so no constraints
      (cons 'and
	    (append (initflips-vab v lp)
		    (list `(implies ,(evar v) (= 0 ,(cons '+ (in-flips v lp))))
			  `(= 1 ,(cons '+ (cons (b->i (evar v)) (list-beginvars v cs))))
			  `(= 1 ,(cons '+ (cons (b->i (evar v)) (list-endvars v cs)))))
			  )))))


;; (check= (init-flips-v '(S . X) '(#\1 #\2))
;; 	'(AND (<= 0 (F X #\1 #\2))
;; 	      (<= (* 2 (F X #\1 #\2)) (L . X))
;; 	      (<= 0 (F X #\2 #\1))
;; 	      (<= (* 2 (F X #\2 #\1)) (L . X))
;; 	      (IMPLIES (E . X)
;; 		  (= 0 (+ (F X #\1 #\2) (F X #\2 #\1))))
;; 	      (= 1 (+ (E . X) (B X #\1) (B X #\2)))
;; 	      (= 1 (+ (E . X) (D X #\1) (D X #\2)))))


