(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)

(assert (= (str.++ y "a" x y "ca") (str.++ "caa" x "a")))

(check-sat)
(get-model)
