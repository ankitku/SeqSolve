(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)

(assert (= (str.++ x "bcccbb" x "bc") (str.++ "a" y "c" z "a" y "c")))

(check-sat)
(get-model)



