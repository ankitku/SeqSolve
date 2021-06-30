(set-logic QF_S)
 
(declare-fun x () String)

(assert (= (str.++ "a" x) x))

(check-sat)
(get-model)
