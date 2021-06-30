(set-logic QF_S)
 
(declare-fun x () String)

(assert (= "abba" "bbba"))

(check-sat)
(get-model)
