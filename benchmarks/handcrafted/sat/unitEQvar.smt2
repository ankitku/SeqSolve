(set-logic QF_S)
(declare-fun w () String)
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)

(assert (= w (str.at x 0)))
(assert (= w x))
(check-sat)
