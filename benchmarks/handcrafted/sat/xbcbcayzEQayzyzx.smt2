(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun v () String)
(declare-fun w () String)

(assert (= (str.++ x "bcbca" y z) (str.++ "a" y z y z x)))

(check-sat)
(get-model)



