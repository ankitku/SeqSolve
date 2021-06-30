(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)

(assert (= (str.++ x "abcd" x "dcba" y) (str.++ "a" y y x)))

(check-sat)
(get-model)
