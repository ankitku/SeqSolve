(set-logic QF_S)
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(assert (= (str.++ x y ) z))
(check-sat)
