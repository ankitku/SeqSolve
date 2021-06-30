(set-logic QF_S)
(declare-fun x () String)
(assert (= (str.++ x "1") (str.++ "1" x)))
(check-sat)
