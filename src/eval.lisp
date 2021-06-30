:q
(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)
(ql:quickload :jsown)
(ql:quickload :trivia)
(ql:quickload :inferior-shell)


(defun read-float (str)
  (with-input-from-string
   (flt-in str) (read flt-in)))

(defun parse-times (str)
  (ppcre:register-groups-bind
   (realtime totalruntime usertime systime pcpu)
   ("(\\d+\\.\\d+).+(\\d+\\.\\d+).+(\\d+\\.\\d+).+(\\d+\\.\\d+).+(\\d+\\.\\d+).+"
    (cl-ppcre:regex-replace-all "\\n" str ""))
   (mapcar 'read-float (list realtime totalruntime usertime systime pcpu))))


(parse-times "Evaluation took:0.388 seconds of real time 0.002085 seconds of total run time (0.000393 user, 0.001692 system) 0.52% CPU 1,046,490,085 processor cycles 0 bytes consed)")


(defun smt-or-smt25p (fl)
  (let ((f (namestring fl)))
    (and (search "smt2" f)
         (not (search "smt20" f)))))

(defun z3str3-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 z3 smt.string_solver=z3str3 ,f)
                                    :TIME t :ON-ERROR nil)
	  (parse-times (get-output-stream-string *trace-output*)))))

(defun z3str2-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 z3 smt.string_solver=z3str2 ,f)
                                    :TIME t :ON-ERROR nil)
	  (parse-times (get-output-stream-string *trace-output*)))))

(defun cvc4-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 cvc4 :strings-exp :lang smt ,f)
                                    :TIME t :ON-ERROR nil)
	  (parse-times (get-output-stream-string *trace-output*)))))

;(cvc4-capture "/Users/ankitku/dev/monoid-solver/evaluation/handcrafted/sat/x1EQsolution.smt2")

(defun norn-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 /Users/ankitku/Downloads/norn/norn ,f)
                                    :TIME t :ON-ERROR nil)
	    (parse-times (get-output-stream-string *trace-output*)))))


(defun sloth-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 /Users/ankitku/Downloads/sloth-master/sloth ,f)
                                    :TIME t :ON-ERROR nil)
	  (parse-times (get-output-stream-string *trace-output*)))))

(defun trau-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 /eval/trau/z3/trau-build/z3 smt.string_solver=trau  ,f)
                                    :TIME t :ON-ERROR nil)
          (parse-times (get-output-stream-string *trace-output*)))))

(defun seqsolve-capture (f)
  (let ((*standard-output* (make-string-output-stream))
        (*trace-output* (make-string-output-stream)))
    (cons (inferior-shell:run/lines `(timeout 60 seqsolve ,f)
                                    :TIME t :ON-ERROR nil)
          (parse-times (get-output-stream-string *trace-output*)))))




;; runs others on all examples in a dir, then creates json output for plot
(defun eval1_dir (tool benchmark n)
  (let* ((bname (car (reverse (cl-ppcre:split "\/" benchmark))))
         (js2 (jsown:new-js
               ("benchmark" bname)
               ("prog_args" "")
               ("program" tool)
               ("prog_alias" tool)))
         (js1 (jsown:new-js
               ("preamble" js2)))
	 (js3 (jsown:new-js)))
    (setq *runs* nil)
    (terpri)
    (cl-fad:walk-directory
     benchmark
     (lambda (fl)
       (let* ((f (namestring fl))
              (fname (cl:file-namestring fl))
              (res (cond ((equal "z3str3" tool) (z3str3-capture f))
                         ((equal "z3str2" tool) (z3str2-capture f))
                         ((equal "cvc4" tool) (cvc4-capture f))
			 ((equal "seqsolve" tool) (seqsolve-capture f))
			 ((equal "norn" tool) (norn-capture f))
			 ((equal "sloth" tool) (sloth-capture f))
			 ((equal "trau" tool) (trau-capture f))
                         (t nil)))
              (out (car res))
              (durs (cdr res))
              (s (in "sat" out))
              (u (in "unsat" out))
	      (e (in "error" out))
              (x (in "unknown" out))
              (tout (not (or s u e x))))
	 ;(print fname)
	 ;(print res)
         (if (or x e) (format t "x")
           (if s (format t "~c[32ms~c[0m" #\ESC #\ESC)
             (if u (format t "~c[31mu~c[0m" #\ESC #\ESC)
	       (if tout (format t "!")
		 nil))))
         (force-output)
         (if (or s u)
             (setf *runs*
                   (cons (append durs (list fname "solved"))
			 *runs*))
           (if (or e x)
	       (setf *runs*
		     (cons (append durs (list fname "unknown"))
			   *runs*))
	     (setf *runs*
                   (cons (append durs (list fname "timedout"))
			 *runs*))))))
     	 :test 'smt-or-smt25p
	 :directories nil)
       (let* ((rs (reduce (lambda (js tf) (jsown:extend-js
				      js
                                        ((sixth tf) (jsown:new-js
						      ("realtime" (first tf))
						      ("totalruntime" (second tf))
						      ("usertime" (third tf))
						      ("systime" (fourth tf))
						      ("res" (seventh tf))
						      ("status" (if (equal (seventh tf) "solved")
								    t
								  :false))))))
			  *runs*
			  :initial-value js3))
	      (resjson (jsown:extend-js
			js1
			("stats" rs)))
	      (jsonfile (format nil "~a-~a-~a.json" bname tool n)))
	 (with-open-file (stream jsonfile
				 :direction :output
				 :if-exists :overwrite
				 :if-does-not-exist :create)
	   (format stream (jsown:to-json resjson))))))
    
    
    
(defun read-json (f)
  (let ((str (uiop:read-file-string f)))
    (cdadr (jsown:parse str "stats"))))

(defun unzip-lists (lop)
  (cond
   ((endp lop) '(() . ()))
   (t (let ((res (unzip-lists (cdr lop))))
    (cons (cons (car (car lop)) (car res))
          (cons (cdr (car lop)) (cdr res)))))))


(defun avg-times (fs)
  (let* ((res (unzip-lists fs))
	 (cleantims (remove nil (cdr res))))
    (cons (in t (car res))
	  (/ (reduce '+ cleantims) (len cleantims)))))



(defun get_rdata (jrun tool)
  (setq *to* 0)
  (setq *un* 0)
  (setq *slvd* 0)
  (setq *tim* 0.0)
  (loop for inst in (cdr jrun)
	do (let* ((jr (cdr inst))
		  (wt (if (equal tool "seqsolve-srv")
			  "rtime"
			"realtime"))
		  (rt (jsown:val jr wt))
		  (res (jsown:val jr "res")))
	     (setq *tim* (+ *tim* rt))
	     (setq *slvd* (+ *slvd* (if (equal res "solved") 1 0)))
	     (setq *un* (+ *un* (if (equal res "unknown") 1 0)))
	     (setq *to* (+ *to* (if (equal res "timedout") 1 0)))))
  (jsown:new-js
	       ("solved" *slvd*)
	       ("total_time" *tim*)
	       ("unknowns" *un*)
	       ("timedout" *to*)))





(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)




(defun shuffled-lists (lst times)
  (loop :repeat times
	:collect (progn (setq clst (copy-list lst))
			(nshuffle clst))))


;(shuffled-lists '(1 2 3 4 5) 10)

(defun avg-time-solved (ls)
  (if (or (endp ls)
	  (equal nil (car ls)))
      nil
    (let* ((res (unzip-lists (mapcar 'car ls)))
	   (avg-time-head (/ (reduce '+ (car res)) (len ls)))
	   (avg-solved-head (/ (len (remove-if-not
				     (lambda (x) x) (cdr res)))
			       (len ls))))
      (cons (cons avg-time-head avg-solved-head)
	    (avg-time-solved (mapcar 'cdr ls))))))


;(avg-time-solved (shuffled-lists '((1 . t) (2 . nil)) 2))


(defun acc-avg-time-solved (accls)
  (cdr (reverse (reduce (lambda (rr run)
			  (cons (cons (+ (car run) (caar rr))
				      (+ (cdr run) (cdar rr)))
				rr))
			accls
			:initial-value '((0 . 0))))))

;(acc-avg-time-solved (avg-time-solved (shuffled-lists '((1 . t) (2 . nil)) 2)))



(defun gen-avg-json (tool bmark n whichtime)
  (setq *avgs* nil)
  (setf *files* nil)
  (cl-fad:walk-directory
   bmark
   (lambda (fl)
     (let* ((fname (enough-namestring fl)))
       (setf *files* (cons fname *files*))))   
   :test 'smt-or-smt25p
   :directories nil)
  (let* ((bname (car (reverse (cl-ppcre:split "\/" bmark))))
	 (selecttime (if (equal tool "seqsolve-srv")
			 "rtime" whichtime))
	 (readj1 (get_rdata (read-json
			     (format nil
				     "~a-~a-~a.json"
				     bname tool 1)) tool))
	 (jss (loop for i from 1 to n
		    collect (let* ((f (format nil
					      "~a-~a-~a.json"
					      bname tool i)))
			      (read-json f))))
	 (makeavg (loop for f in *files*
			do (let* ((status-rtimes (mapcar
						  (lambda (js) (ignore-errors
							    (let ((tgt (jsown:val js (cl:file-namestring f))))
							      (cons (jsown:val tgt "status")
								    (jsown:val tgt selecttime)))))
						  jss))
				  (avgtime (avg-times status-rtimes)))
			     (setf *avgs*
				   (cons (cons (cdr avgtime) (car avgtime)) *avgs*)))))
	 (acc (acc-avg-time-solved (avg-time-solved (shuffled-lists *avgs* 100))))
	 (js2 (jsown:new-js
	       ("benchmark" bname)
	       ("prog_args" "")
	       ("program" tool)
	       ("prog_alias" tool)))
	 (js1 (jsown:new-js
	       ("preamble" js2)
	       ("rundata" readj1)))
	 (js3 (jsown:new-js))
	 (rs (reduce (lambda (js tf) (jsown:extend-js
				 js
				 ((car tf) (jsown:new-js
					    ("rtime" (car tf))
					    ("solved" (cdr tf))))))
		     acc
		     :initial-value js3))
	 (resjson (jsown:extend-js
		   js1
		   ("stats" rs)))
	 (jsonfile (format nil "~a-avg-~a.json" bname tool)))
    (with-open-file (stream jsonfile
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (format stream (jsown:to-json resjson)))))






    			    

(defun eval-tool (args)
  (let* ((tool (first args))
	 (bmark (second args))
	 (iter (parse-integer (third args)))
	 (whichtime (fourth args)))
    (if (equal whichtime nil)
	(loop for i from 1 to iter 
	      do (eval1_dir tool bmark i))
      (gen-avg-json tool bmark iter whichtime))
    (sb-ext:exit)))




(save-exec "eval_tool_exec" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(eval-tool (cdr sb-ext:*posix-argv*))' --disable-debugger")



