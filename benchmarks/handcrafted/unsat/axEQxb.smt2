(set-logic QF_S)
 
(declare-fun x () String)

(assert (= (str.++ "a" x) (str.++ x "b")))

(check-sat)
(get-model)
