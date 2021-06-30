(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)

(assert (= (str.++  "a" x "c" y "b") (str.++ "y" "cba" x)))

(check-sat)
(get-model)
