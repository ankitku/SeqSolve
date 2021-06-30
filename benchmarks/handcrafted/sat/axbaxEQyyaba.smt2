(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)

(assert (= (str.++ "a" x "ba" x) (str.++ y y "aba")))

(check-sat)
(get-model)

