(set-logic QF_S)
(declare-fun e1 () String)
(declare-fun e2 () String)
(declare-fun e3 () String)
(declare-fun e4 () String)
(declare-fun e5 () String)
(declare-fun e6 () String)
(declare-fun e7 () String)
(declare-fun e8 () String)
(declare-fun e9 () String)
(declare-fun e10 () String)
(assert (= 1 (str.len e1)))
(assert (= 1 (str.len e2)))
(assert (= 1 (str.len e3)))
(assert (= 1 (str.len e4)))
(assert (= 1 (str.len e5)))
(assert (= 1 (str.len e6)))
(assert (= 1 (str.len e7)))
(assert (= 1 (str.len e8)))
(assert (= 1 (str.len e9)))
(assert (= 1 (str.len e10)))
(assert (= (str.++ e1 e2 e3 e4 e5) (str.++ e6 e7 e8 "123" e9 e10)))
(check-sat)
