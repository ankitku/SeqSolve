(set-logic QF_S)
(declare-fun e () String)
(declare-fun x () String)
(assert (= "11" (str.++ x "1")))
(check-sat)
