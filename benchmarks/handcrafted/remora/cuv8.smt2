(set-logic QF_S)
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun w () String)
(assert (= (str.++ "23" x y z "3") x))
(check-sat)
