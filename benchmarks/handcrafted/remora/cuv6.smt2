(set-logic QF_S)
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun w () String)
(assert (= (str.++ x y z "3") (str.++ "1" w "3")))
(check-sat)
