(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)

(assert (= (str.++ x "b" y z z "b" x "ba") (str.++ y "a" z y "b" z "b" y y)))

(check-sat)
(get-model)
