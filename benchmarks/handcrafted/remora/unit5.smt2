(set-logic QF_S)
(declare-fun e () String)
(declare-fun x () String)
(assert (= 1 (str.len e)))
(assert (= (str.++ e "1") (str.++ x "2")))
(check-sat)
