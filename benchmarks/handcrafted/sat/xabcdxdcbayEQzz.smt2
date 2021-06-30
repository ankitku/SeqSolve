(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)

(assert (= (str.++ x "abcd" x "dcba" y) (str.++ z z)))

(check-sat)
(get-model)

